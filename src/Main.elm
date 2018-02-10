module Main exposing (..)

import AnimationFrame
import Common exposing (..)
import Game.TwoD.Camera exposing (viewportToGameCoordinates)
import Html
import Math
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
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


init : Flag -> ( Model, Cmd Msg )
init { windowWidth, windowHeight, timestamp } =
    let
        seed =
            Random.initialSeed timestamp

        ( enemies, newSeed ) =
            --initEnemies seed
            ( [], seed )
    in
    ( { windowWidth = windowWidth
      , windowHeight = windowHeight
      , egg =
            { pos = V2.fromTuple ( 0, 0 )
            , rad = 10
            }
      , hero =
            { state = Sword --Shield
            , pos = V2.fromTuple ( 100, 50 )
            , lastPos = V2.fromTuple ( 100, 50 )
            , length = 50
            , angle = 0
            , lastAngle = 0
            , thickness = 5
            }
      , enemies = enemies
      , isGameOver = False
      , seed = newSeed
      , timeSinceLastSpawn = 0
      , curTime = 0
      , config =
            { isPaused = True
            }
      }
    , Cmd.none
    )


initEnemies : Random.Seed -> ( List Enemy, Random.Seed )
initEnemies seed =
    -- for debugging mainly
    let
        spacing =
            10

        frac =
            8
    in
    List.range (round (cameraWidth / frac) // -spacing) (round (cameraWidth / frac) // spacing)
        |> List.map
            (\w ->
                List.range (round (cameraHeight / frac) // -spacing) (round (cameraHeight / frac) // spacing)
                    |> List.map
                        (\h ->
                            { pos = V2.fromTuple ( toFloat w * spacing, toFloat h * spacing )
                            , lastPos = V2.fromTuple ( toFloat w * spacing, toFloat h * spacing )
                            , rad = 2
                            }
                        )
            )
        |> List.concat
        |> (\e -> ( e, seed ))



--Random.step (Random.list 10 enemyGenerator) seed


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
update msg ({ windowWidth, windowHeight, hero, config } as model) =
    case msg of
        MouseClick mousePos ->
            ({ model | hero = toggleState hero } |> mouseMove mousePos) ! []

        MouseMove mousePos ->
            mouseMove mousePos model ! []

        Tick timeDelta ->
            -- max time delta is 30 FPS (1000 / 30 == 33)
            tick (min timeDelta 33) model ! []

        TogglePause ->
            let
                newConfig =
                    { config | isPaused = not config.isPaused }
            in
            { model | config = newConfig } ! []


mouseMove : Mouse.Position -> Model -> Model
mouseMove { x, y } ({ windowWidth, windowHeight } as model) =
    viewportToGameCoordinates
        camera
        ( cameraWidth, cameraHeight )
        ( round <| toFloat x - 0.5 * (toFloat windowWidth - cameraWidth)
        , round <| toFloat y - 0.5 * (toFloat windowHeight - cameraHeight)
        )
        |> (\( gameX, gameY ) ->
                moveHero (V2.fromTuple ( gameX, gameY )) model.egg.pos model
           )


moveHero : Vec2 -> Vec2 -> Model -> Model
moveHero mousePos eggPos ({ hero } as model) =
    let
        ( eggX, eggY ) =
            V2.toTuple eggPos

        ( mouseX, mouseY ) =
            V2.toTuple mousePos

        ( pos, angle ) =
            case hero.state of
                Shield ->
                    toPolar ( eggX - mouseX, eggY - mouseY )
                        |> (\( _, angle ) ->
                                ( mousePos, angle + turns 0.25 )
                           )

                Sword ->
                    toPolar ( eggX - mouseX, eggY - mouseY )
                        |> (\( _, angle ) ->
                                ( fromPolar ( hero.length / 2, angle )
                                    |> (\( x, y ) ->
                                            V2.fromTuple ( mouseX - x, mouseY - y )
                                       )
                                , angle
                                )
                           )
    in
    { model
        | hero =
            { hero
                | pos = pos
                , lastPos = hero.pos
                , angle = angle
                , lastAngle = hero.angle
            }
    }


initTimeToSpawn =
    500


tick : Time -> Model -> Model
tick timeDelta ({ egg, enemies, hero, timeSinceLastSpawn, seed } as model) =
    let
        curTime =
            model.curTime + timeDelta

        timeToSpawn =
            -- TODO get faster and faster
            initTimeToSpawn

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
                |> List.map (moveEnemyCloserToEgg timeDelta egg)
                |> List.filterMap (doesCollideWithHero hero)

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
    }


doesCollideWithEgg : Egg -> Enemy -> Bool
doesCollideWithEgg egg enemy =
    let
        dist_ =
            Math.dist egg.pos enemy.pos
    in
    dist_ < (egg.rad + enemy.rad)


doesCollideWithHero : Hero -> Enemy -> Maybe Enemy
doesCollideWithHero hero enemy =
    if isTouchingHero hero enemy then
        Nothing
    else
        Just enemy



--let
--    dist_ =
--        dist hero.pos enemy.pos
--in
--if dist_ < (hero.rad + enemy.rad) then
--    Nothing
--else
--    Just enemy


moveEnemyCloserToEgg : Time -> Egg -> Enemy -> Enemy
moveEnemyCloserToEgg timeDelta egg enemy =
    let
        ( eggX, eggY ) =
            V2.toTuple egg.pos

        ( enemyX, enemyY ) =
            V2.toTuple enemy.pos
    in
    toPolar ( eggX - enemyX, eggY - enemyY )
        |> (\( _, angle ) -> fromPolar ( timeDelta * enemySpeed, angle ))
        |> (\( x, y ) ->
                { enemy
                    | pos = V2.fromTuple ( enemyX + x, enemyY + y )
                    , lastPos = enemy.pos
                }
           )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions ({ isGameOver, config } as model) =
    Sub.batch
        [ Mouse.moves MouseMove
        , Mouse.clicks MouseClick
        , if isGameOver || config.isPaused then
            Sub.none
          else
            AnimationFrame.diffs Tick
        ]


isTouchingHero : Hero -> Enemy -> Bool
isTouchingHero hero enemy =
    -- check if distance between
    let
        ( a, b, c, d ) =
            getHeroSweepQuadPoints hero

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
    (minDist <= (hero.thickness / 2) + enemy.rad)
        || (List.length (List.filter (Math.doLinesIntersect ( enemy.pos, V2.fromTuple ( -1000, -1000 ) )) [ ( a, b ), ( b, c ), ( c, d ), ( d, a ) ]) % 2 == 1)
