module Init exposing (init)

import Common exposing (..)
import Math.Vector2 as V2 exposing (Vec2)
import Random


startingPos =
    V2.vec2 0 50


init : Flag -> ( Model, Cmd Msg )
init { cameraWidth, cameraHeight, timestamp } =
    let
        seed =
            Random.initialSeed timestamp

        ( enemies, newSeed ) =
            initEnemies ( cameraWidth, cameraHeight ) seed

        --( [], seed )
    in
    ( { cameraWidth = cameraWidth
      , cameraHeight = cameraHeight
      , egg =
            { pos = V2.fromTuple ( 0, 0 )
            , rad = 10
            }
      , hero =
            { state = Shield -- Sword
            , pos = startingPos
            , lastPos = startingPos
            , length = 1
            , angle = 0
            , lastAngle = 0
            , thickness = 1
            }
      , enemies = enemies
      , isGameOver = False
      , seed = newSeed
      , timeSinceLastSpawn = 0
      , curTime = 0
      , config =
            if True then
                -- for debugging
                { isPaused = False
                , heroLength = 15
                , heroThickness = 1
                , enemySpeed = 0
                , enemySpawnRate = 0
                }
            else
                -- kinda good
                { isPaused = True
                , heroLength = 15
                , heroThickness = 1
                , enemySpeed = 2
                , enemySpawnRate = 2
                }
      , mousePos = startingPos
      }
    , Cmd.none
    )


initEnemies : ( Int, Int ) -> Random.Seed -> ( List Enemy, Random.Seed )
initEnemies ( cameraWidth, cameraHeight ) seed =
    -- for debugging mainly
    let
        spacing =
            10

        frac =
            8
    in
    List.range (round (toFloat cameraWidth / frac) // -spacing) (round (toFloat cameraWidth / frac) // spacing)
        |> List.map
            (\w ->
                List.range (round (toFloat cameraHeight / frac) // -spacing) (round (toFloat cameraHeight / frac) // spacing)
                    |> List.map
                        (\h ->
                            { pos = V2.fromTuple ( toFloat w * spacing, toFloat h * spacing )
                            , lastPos = V2.fromTuple ( toFloat w * spacing, toFloat h * spacing )
                            , rad = 2
                            , state = Alive
                            , seed = seed
                            }
                        )
            )
        |> List.concat
        |> (\e -> ( e, seed ))



--Random.step (Random.list 10 enemyGenerator) seed
