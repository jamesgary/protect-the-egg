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
        MouseClick _ ->
            { model | hero = toggleState hero } ! []

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


heroSpeed =
    0.3


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


moveHero : Time -> Model -> Hero
moveHero timeDelta ({ config, hero, egg, mousePos } as model) =
    let
        ( eggX, eggY ) =
            V2.toTuple egg.pos

        -- hilt is the mouse-controllable point
        hiltPos =
            hiltPosFromHero config hero

        ( mouseX, mouseY ) =
            V2.toTuple mousePos

        maxDist =
            heroSpeed * timeDelta

        newHiltPos =
            V2.distance mousePos hiltPos
                |> (\dist ->
                        if dist > maxDist then
                            V2.sub hiltPos mousePos
                                |> V2.scale (maxDist / dist)
                                |> V2.sub hiltPos
                        else
                            mousePos
                   )

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
                ( Random.step (Random.list numEnemiesToSpawnInt enemyGenerator) seed
                , curTime - timePassedSinceLastSpawn
                )
            else
                ( ( [], seed ), timeSinceLastSpawn )

        movedEnemies =
            enemies
                |> List.append spawnedEnemies
                |> List.map (moveEnemyCloserToEgg config timeDelta egg)
                |> List.map (collideWithHero config curTime movedHero)
                |> List.filter (isAlive config curTime)

        isGameOver =
            List.any (doesCollideWithEgg egg) movedEnemies
                -- FIXME
                |> always False
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

        Exploding expTime ->
            expTime > curTime


doesCollideWithEgg : Egg -> Enemy -> Bool
doesCollideWithEgg egg enemy =
    let
        dist_ =
            Math.dist egg.pos enemy.pos
    in
    dist_ < (egg.rad + enemy.rad)


collideWithHero : Config -> Time -> Hero -> Enemy -> Enemy
collideWithHero config curTime hero enemy =
    case enemy.state of
        Alive ->
            if isTouchingHero config hero enemy then
                { enemy | state = Exploding (curTime + explosionLongevity) }
            else
                enemy

        Exploding _ ->
            enemy



--let
--    dist_ =
--        dist hero.pos enemy.pos
--in
--if dist_ < (hero.rad + enemy.rad) then
--    Nothing
--else
--    Just enemy


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
