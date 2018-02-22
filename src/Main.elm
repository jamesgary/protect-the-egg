module Main exposing (..)

import AnimationFrame
import Common exposing (..)
import Game.TwoD.Camera exposing (viewportToGameCoordinates)
import Html
import Init exposing (init)
import Math
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
import Ports exposing (windowChanged)
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


enemyGenerator : Random.Generator Enemy
enemyGenerator =
    Random.float 0 (turns 1)
        |> Random.map
            (\angle ->
                fromPolar ( enemyStartingDistFromEgg, angle )
                    |> (\( x, y ) ->
                            { pos = V2.fromTuple ( x, y )
                            , lastPos = V2.fromTuple ( x, y )
                            , rad = 2
                            , state = Alive
                            , seed = Random.initialSeed (round (angle * toFloat Random.maxInt))
                            }
                       )
            )


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
                                        }
                                   )
                        )
            )



-- UPDATE


toggleState : Hero -> Hero
toggleState hero =
    case hero.state of
        Sword ->
            { hero | state = Shield }

        Shield ->
            { hero | state = Sword }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ cameraWidth, cameraHeight, hero, config } as model) =
    case msg of
        MouseClick mousePos ->
            { model | hero = toggleState hero |> (\hero -> { hero | pos = trueMousePos model mousePos }) } ! []

        MouseMove mousePos ->
            { model | mousePos = trueMousePos model mousePos } ! []

        Tick timeDelta ->
            -- max time delta is 30 FPS (1000 / 30 == 33)
            tick (min timeDelta 33) model ! []

        WindowChanged ( width, height ) ->
            { model | cameraWidth = width, cameraHeight = height } ! []

        TogglePause ->
            let
                newConfig =
                    { config | isPaused = not config.isPaused }
            in
            { model | config = newConfig } ! []

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


trueMousePos : Model -> Mouse.Position -> Vec2
trueMousePos { cameraWidth, cameraHeight } { x, y } =
    let
        w =
            toFloat cameraWidth

        h =
            toFloat cameraHeight

        ( innerWidth, innerHeight, xOffset, yOffset ) =
            if w / h > 16 / 9 then
                -- too wide!
                ( h * (16 / 9), h, w - (h * 16 / 9), 0 )
            else
                -- too tall!
                ( w, w * (9 / 16), 0, h - (w * (9 / 16)) )
    in
    viewportToGameCoordinates
        camera
        ( innerWidth |> round, innerHeight |> round )
        ( round <| toFloat x - 0.5 * xOffset
        , round <| toFloat y - 0.5 * yOffset
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


moveHero : Time -> Model -> Hero
moveHero timeDelta ({ config, hero, egg, mousePos } as model) =
    let
        ( eggX, eggY ) =
            V2.toTuple egg.pos

        -- hilt is the mouse-controllable point
        hiltPos =
            hiltPosFromHero config hero

        ( newHiltPos, newVel ) =
            moveHilt config timeDelta hiltPos hero.vel mousePos

        angle =
            case hero.state of
                Shield ->
                    V2.sub egg.pos newHiltPos
                        |> V2.toTuple
                        |> toPolar
                        |> (\( _, angle ) ->
                                angle + turns 0.25
                           )

                Sword ->
                    V2.sub egg.pos newHiltPos
                        |> V2.toTuple
                        |> toPolar
                        |> (\( _, angle ) ->
                                angle + turns 0.5
                           )
    in
    { hero
        | pos = heroPosFromHilt config { hero | angle = angle } newHiltPos
        , lastPos = hero.pos
        , vel = newVel
        , angle = angle
        , lastAngle = hero.angle
    }


tick : Time -> Model -> Model
tick timeDelta ({ config, egg, enemies, hero, timeSinceLastSpawn, seed, mousePos } as model) =
    let
        curTime =
            model.curTime + timeDelta

        -- MOVE HERO
        movedHero =
            moveHero timeDelta model

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

        movedEnemies =
            enemies
                |> List.append (spawnedEnemies |> List.concat)
                |> List.map (moveEnemyCloserToEgg config timeDelta egg)
                |> List.map (collideWithHero config curTime movedHero)
                |> List.filter (isAlive config curTime)

        isGameOver =
            List.any (doesCollideWithEgg egg) movedEnemies

        -- FIXME
        --|> always False
    in
    { model
        | enemies = movedEnemies
        , isGameOver = isGameOver
        , curTime = curTime
        , timeSinceLastSpawn = newTimeSinceLastSpawn
        , seed = newSeed
        , hero = movedHero
    }


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


doesCollideWithEgg : Egg -> Enemy -> Bool
doesCollideWithEgg egg enemy =
    case enemy.state of
        Alive ->
            Math.dist egg.pos enemy.pos < (egg.rad + enemy.rad)

        _ ->
            False


collideWithHero : Config -> Time -> Hero -> Enemy -> Enemy
collideWithHero config curTime hero enemy =
    case enemy.state of
        Alive ->
            if isTouchingHero config hero enemy then
                --{ enemy | state = Exploding (curTime + explosionLongevity) }
                { enemy
                    | state =
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

                    --hero.angle
                }
            else
                enemy

        Bouncing _ ->
            enemy

        Exploding _ ->
            enemy


baseSpeed =
    0.02


moveEnemyCloserToEgg : Config -> Time -> Egg -> Enemy -> Enemy
moveEnemyCloserToEgg config timeDelta egg enemy =
    case enemy.state of
        Alive ->
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
subscriptions ({ isGameOver, config } as model) =
    Sub.batch
        [ Mouse.moves MouseMove
        , Mouse.clicks MouseClick
        , windowChanged WindowChanged
        , if isGameOver || config.isPaused then
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
