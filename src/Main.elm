module Main exposing (..)

import AnimationFrame
import Common exposing (..)
import ElementRelativeMouseEvents as Mouse
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera exposing (viewportToGameCoordinates)
import Html
import Init exposing (init)
import Math
import Math.Vector2 as V2 exposing (Vec2)
import Ports exposing (playWav, windowChanged)
import Random
import Time exposing (Time)
import View exposing (view)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


enemySpeed =
    0.02


enemyStartingDistFromEgg =
    150


clusterGenerator : Config -> Random.Generator (List Enemy)
clusterGenerator { enemyClusterSize } =
    Random.float 0 (turns 1)
        |> Random.map
            (\angle ->
                List.range 0 (enemyClusterSize - 1)
                    |> List.map
                        (\i ->
                            fromPolar ( enemyStartingDistFromEgg + (5 * toFloat i), angle )
                                |> (\( x, y ) ->
                                        { pos = V2.fromTuple ( x, y )
                                        , lastPos = V2.fromTuple ( x, y )
                                        , rad = 2
                                        , state = Alive
                                        , seed = Random.initialSeed (round (angle * toFloat Random.maxInt * toFloat i))
                                        , lastAteAt = 0
                                        }
                                   )
                        )
            )


toggleState : Hero -> Hero
toggleState ({ state, angle } as hero) =
    case state of
        Sword ->
            { hero | state = Shield, angle = angle - turns 0.25 }

        Shield ->
            { hero | state = Sword, angle = angle + turns 0.25 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ viewportWidth, viewportHeight, hero, config, isPaused } as model) =
    case msg of
        MouseClick mousePos ->
            { model
                | hero =
                    toggleState hero
                        |> (\hero -> { hero | pos = trueMousePos model mousePos })
            }
                ! []

        MouseMove mousePos ->
            { model | mousePos = trueMousePos model mousePos } ! []

        Tick timeDelta ->
            -- max time delta is 30 FPS (1000 / 30 == 33)
            tick (min timeDelta 33) model

        WindowChanged ( width, height ) ->
            let
                ( canvasSize, sidebarWidth ) =
                    getDims width height
            in
            { model
                | viewportWidth = width
                , viewportHeight = height
                , canvasSize = canvasSize
                , sidebarWidth = sidebarWidth
            }
                ! []

        TogglePause ->
            { model | isPaused = not isPaused } ! []

        ChangeHeroLength inputStr ->
            let
                newConfig =
                    { config
                        | heroLength =
                            String.toFloat inputStr
                                |> Result.withDefault config.heroLength
                    }
            in
            { model | config = newConfig } ! []

        ChangeHeroThickness inputStr ->
            let
                newConfig =
                    { config
                        | heroThickness =
                            String.toFloat inputStr
                                |> Result.withDefault config.heroThickness
                    }
            in
            { model | config = newConfig } ! []

        ChangeEnemySpeed inputStr ->
            let
                newConfig =
                    { config
                        | enemySpeed =
                            String.toFloat inputStr
                                |> Result.withDefault config.enemySpeed
                    }
            in
            { model | config = newConfig } ! []

        ChangeEnemySpawnRate inputStr ->
            let
                newConfig =
                    { config
                        | enemySpawnRate =
                            String.toFloat inputStr
                                |> Result.withDefault config.enemySpawnRate
                    }
            in
            { model | config = newConfig } ! []

        ChangeEnemyClusterSize inputStr ->
            let
                newConfig =
                    { config
                        | enemyClusterSize =
                            String.toInt inputStr
                                |> Result.withDefault config.enemyClusterSize
                    }
            in
            { model | config = newConfig } ! []

        Resources msg ->
            { model | resources = Resources.update msg model.resources } ! []

        MouseOnStartBtn ->
            { model | isStartBtnHovered = True } ! []

        MouseOutStartBtn ->
            { model | isStartBtnHovered = False } ! []

        StartGame ->
            { model | isPaused = False, state = Playing } ! []


trueMousePos : Model -> Mouse.Point -> Vec2
trueMousePos { viewportWidth, viewportHeight } { x, y } =
    let
        w =
            toFloat viewportWidth

        h =
            toFloat viewportHeight

        ( innerWidth, innerHeight, xOffset, yOffset ) =
            if w - h > 0 then
                -- too wide!
                ( h, h, w - h, 0 )
            else
                -- too tall!
                ( w, w, 0, h - w )
    in
    viewportToGameCoordinates
        camera
        ( innerWidth |> round, innerHeight |> round )
        ( round <| x
        , round <| y
        )
        |> (\( gameX, gameY ) ->
                V2.fromTuple ( gameX, gameY )
           )


hiltPosFromHero : Config -> Hero -> Vec2
hiltPosFromHero config hero =
    case hero.state of
        Shield ->
            hero.pos

        Sword ->
            fromPolar ( trueLength config hero / 2, hero.angle )
                |> V2.fromTuple
                |> V2.sub hero.pos


heroPosFromHilt : Config -> Hero -> Vec2 -> Vec2
heroPosFromHilt config hero hiltPos =
    case hero.state of
        Shield ->
            hiltPos

        Sword ->
            fromPolar ( trueLength config hero / 2, hero.angle )
                |> V2.fromTuple
                |> V2.add hiltPos


stiffness =
    170


damping =
    20


pTolerance =
    0.01


vTolerance =
    0.01


moveHilt : Config -> Time -> Vec2 -> Vec2 -> Vec2 -> ( Vec2, Vec2 )
moveHilt config timeDelta oldPos oldVel targetPos =
    -- From https://github.com/mdgriffith/elm-style-animation/blob/
    -- 86f81b0f5a28289894fe61c14fa2c34c0bf895ec/src/Animation/Model.elm#L1591-L1623
    let
        fspring =
            V2.sub targetPos oldPos
                |> V2.scale stiffness

        fdamper =
            oldVel
                |> V2.scale -damping

        a =
            V2.add fspring fdamper

        newVelocity =
            V2.add oldVel (V2.scale (timeDelta / 1000) a)

        newPos =
            V2.add oldPos (V2.scale (timeDelta / 1000) newVelocity)

        dx =
            V2.distance targetPos newPos
    in
    if dx < pTolerance && V2.length newVelocity < vTolerance then
        ( targetPos, V2.fromTuple ( 0, 0 ) )
    else
        ( newPos, newVelocity )


moveHero : Time -> Model -> Model
moveHero timeDelta ({ config, hero, egg, mousePos } as model) =
    let
        -- hilt is the mouse-controllable point
        hiltPos =
            hiltPosFromHero config hero

        ( newHiltPos, newVel ) =
            moveHilt config timeDelta hiltPos hero.vel mousePos

        aroundEggPos =
            newHiltPos
                |> V2.distance egg.pos
                |> (\d ->
                        if d < nestRad then
                            V2.direction newHiltPos egg.pos
                                |> V2.normalize
                                |> V2.scale nestRad
                        else if d > beachRad then
                            V2.direction newHiltPos egg.pos
                                |> V2.normalize
                                |> V2.scale beachRad
                        else
                            newHiltPos
                   )

        angle =
            case hero.state of
                Shield ->
                    V2.sub egg.pos aroundEggPos
                        |> V2.toTuple
                        |> toPolar
                        |> (\( _, angle ) ->
                                angle + turns 0.25
                           )

                Sword ->
                    V2.sub egg.pos aroundEggPos
                        |> V2.toTuple
                        |> toPolar
                        |> (\( _, angle ) ->
                                angle + turns 0.5
                           )

        newHero =
            { hero
                | pos = heroPosFromHilt config { hero | angle = angle } aroundEggPos
                , lastPos = hero.pos
                , vel = newVel
                , angle = angle
                , lastAngle = hero.angle
            }
    in
    { model | hero = newHero }


tick : Time -> Model -> ( Model, Cmd Msg )
tick timeDelta ({ config, egg, enemies, hero, timeSinceLastSpawn, seed, mousePos } as model) =
    model
        |> updateCurTime timeDelta
        |> moveHero timeDelta
        |> spawnEnemies
        |> moveEnemies timeDelta
        |> collideHeroAndEnemies
        |> removeDeadEnemies
        |> eatEggs
        |> checkGameOver
        |> cmdify


cmdify : Model -> ( Model, Cmd Msg )
cmdify ({ cmds } as model) =
    { model | cmds = [] } ! cmds


updateCurTime : Time -> Model -> Model
updateCurTime timeDelta ({ curTime } as model) =
    { model | curTime = curTime + timeDelta }


spawnEnemies : Model -> Model
spawnEnemies ({ enemies, qEnemies, curTime, effects, cmds } as model) =
    case qEnemies of
        [] ->
            model

        ( timeToSpawn, enemy ) :: tail ->
            if timeToSpawn <= curTime then
                spawnEnemies
                    { model
                        | enemies = enemy :: enemies
                        , qEnemies = tail
                        , cmds = playWav "crab-hello" :: cmds
                        , effects =
                            { expTime = curTime + splashLongevity
                            , pos = enemy.pos
                            , kind = Splash
                            , seed = enemy.seed
                            }
                                :: effects
                    }
            else
                model


moveEnemies : Time -> Model -> Model
moveEnemies timeDelta ({ enemies, config, egg } as model) =
    { model | enemies = List.map (moveEnemyCloserToEgg config timeDelta egg) enemies }


collideHeroAndEnemies : Model -> Model
collideHeroAndEnemies ({ config, egg, hero, enemies, curTime, kaiju, cmds } as model) =
    let
        ( newEnemies, numCollidedEnemies ) =
            enemies
                |> List.map (collideWithHero config curTime hero)
                |> (\listOfEnemiesAndDidCollide ->
                        ( listOfEnemiesAndDidCollide
                            |> List.map Tuple.first
                        , listOfEnemiesAndDidCollide
                            |> List.filter Tuple.second
                            |> List.length
                        )
                   )

        newHero =
            if numCollidedEnemies > 0 then
                bumpHero egg hero
            else
                hero
    in
    { model
        | enemies = newEnemies
        , hero = newHero
        , kaiju = kaiju + numCollidedEnemies
        , cmds =
            if numCollidedEnemies > 0 then
                playWav "crab-death" :: cmds
            else
                cmds
    }


removeDeadEnemies : Model -> Model
removeDeadEnemies ({ enemies, config, curTime } as model) =
    { model | enemies = List.filter (isAlive config curTime) enemies }


munchTime =
    1000


eatEggs : Model -> Model
eatEggs ({ egg, enemies, curTime, numEggs } as model) =
    enemies
        |> List.map
            (\quab ->
                if doesCollideWithNest quab && (quab.lastAteAt + munchTime < curTime) then
                    ( { quab | lastAteAt = curTime }, True )
                else
                    ( quab, False )
            )
        |> (\enemiesAndDidEat ->
                { model
                    | enemies = List.map Tuple.first enemiesAndDidEat
                    , numEggs = numEggs - (enemiesAndDidEat |> List.filter Tuple.second |> List.length)
                }
           )


checkGameOver : Model -> Model
checkGameOver ({ numEggs } as model) =
    { model | isGameOver = numEggs <= 0 }



{- -- FOR ENDLESS MODE?

   -- SPAWN ENEMIES
   timeToSpawn =
       1000 / config.enemySpawnRate

   numEnemiesToSpawnFloat =
       (curTime - timeSinceLastSpawn) / timeToSpawn

   numEnemiesToSpawnInt =
       floor numEnemiesToSpawnFloat

   timePassedSinceLastSpawn =
       (numEnemiesToSpawnFloat - toFloat numEnemiesToSpawnInt) * timeToSpawn

   ( ( spawnedEnemies, newSeed ), newTimeSinceLastSpawn ) =
       if numEnemiesToSpawnInt >= 1 then
           ( Random.step (Random.list numEnemiesToSpawnInt (clusterGenerator config)) seed
           , curTime - timePassedSinceLastSpawn
           )
       else
           ( ( [], seed ), timeSinceLastSpawn )

   ( movedEnemies, didCollide ) =
       enemies
           |> List.append (spawnedEnemies |> List.concat)
           |> List.map (moveEnemyCloserToEgg config timeDelta egg)
           |> List.map (collideWithHero config curTime movedHero)
           |> (\listOfEnemiesAndDidCollide ->
                   ( listOfEnemiesAndDidCollide
                       |> List.map Tuple.first
                       |> List.filter (isAlive config curTime)
                   , List.any Tuple.second listOfEnemiesAndDidCollide
                   )
              )
-}


isAlive : Config -> Time -> Enemy -> Bool
isAlive config curTime enemy =
    case enemy.state of
        Alive ->
            True

        Bouncing _ ->
            -- TODO check offscreen
            True

        Exploding expTime ->
            expTime > curTime


eggPos =
    V2.fromTuple ( 0, 0 )


eggRad =
    10


doesCollideWithNest : Enemy -> Bool
doesCollideWithNest enemy =
    case enemy.state of
        Alive ->
            Math.dist eggPos enemy.pos < (eggRad + enemy.rad)

        _ ->
            False


collideWithHero : Config -> Time -> Hero -> Enemy -> ( Enemy, Bool )
collideWithHero config curTime hero enemy =
    case enemy.state of
        Alive ->
            if isTouchingHero config hero enemy then
                ( { enemy
                    | state =
                        if True then
                            Exploding (curTime + explosionLongevity)
                        else
                            Bouncing
                                (V2.sub enemy.pos enemy.lastPos
                                    |> V2.negate
                                    |> V2.toTuple
                                    |> toPolar
                                    |> (\( r, a ) ->
                                            ((hero.angle + turns 0.25) - a)
                                                + (hero.angle + turns 0.25)
                                       )
                                )
                  }
                , True
                )
            else
                ( enemy, False )

        Bouncing _ ->
            ( enemy, False )

        Exploding _ ->
            ( enemy, False )


baseSpeed =
    0.01


bumpHero : Egg -> Hero -> Hero
bumpHero egg hero =
    let
        ( eggX, eggY ) =
            V2.toTuple egg.pos

        ( heroX, heroY ) =
            V2.toTuple hero.pos
    in
    toPolar ( eggX - heroX, eggY - heroY )
        |> (\( _, angle ) -> fromPolar ( 3, angle ))
        |> (\( x, y ) ->
                { hero | pos = V2.fromTuple ( heroX + x, heroY + y ) }
           )


moveEnemyCloserToEgg : Config -> Time -> Egg -> Enemy -> Enemy
moveEnemyCloserToEgg config timeDelta egg enemy =
    case enemy.state of
        Alive ->
            if doesCollideWithNest enemy then
                enemy
            else
                let
                    ( eggX, eggY ) =
                        V2.toTuple egg.pos

                    ( enemyX, enemyY ) =
                        V2.toTuple enemy.pos
                in
                toPolar ( eggX - enemyX, eggY - enemyY )
                    |> (\( _, angle ) -> fromPolar ( timeDelta * config.enemySpeed * baseSpeed, angle ))
                    |> (\( x, y ) ->
                            { enemy
                                | pos = V2.fromTuple ( enemyX + x, enemyY + y )
                                , lastPos = enemy.pos
                            }
                       )

        Bouncing angle ->
            { enemy
                | pos =
                    fromPolar ( timeDelta * config.enemySpeed * baseSpeed, angle )
                        |> V2.fromTuple
                        |> V2.add enemy.pos
                , lastPos = enemy.pos
            }

        _ ->
            enemy



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions ({ isGameOver, isPaused } as model) =
    Sub.batch
        [ windowChanged WindowChanged
        , if isGameOver || isPaused then
            Sub.none
          else
            AnimationFrame.diffs Tick
        ]


isTouchingHero : Config -> Hero -> Enemy -> Bool
isTouchingHero ({ heroLength, heroThickness } as config) hero enemy =
    -- check if distance between
    let
        ( a, b, c, d ) =
            getHeroSweepQuadPoints config hero

        ( e, f ) =
            ( enemy.pos, enemy.lastPos )

        minDist =
            List.minimum
                [ Math.getDistBetweenLines ( a, b ) ( e, f )
                , Math.getDistBetweenLines ( b, c ) ( e, f )
                , Math.getDistBetweenLines ( c, d ) ( e, f )
                , Math.getDistBetweenLines ( d, a ) ( e, f )
                ]
                |> Maybe.withDefault -42
    in
    (minDist <= (heroThickness * hero.thickness / 2) + enemy.rad)
        || (List.length (List.filter (Math.doLinesIntersect ( enemy.pos, V2.fromTuple ( -1000, -1000 ) )) [ ( a, b ), ( b, c ), ( c, d ), ( d, a ) ]) % 2 == 1)
