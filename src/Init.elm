module Init exposing (init)

import Common exposing (..)
import Game.Resources as Resources exposing (Resources)
import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)


startingPos =
    V2.vec2 0 50


init : Flag -> ( Model, Cmd Msg )
init { viewportWidth, viewportHeight, timestamp } =
    let
        seed =
            Random.initialSeed timestamp

        ( enemies, newSeed ) =
            --initEnemies ( viewportWidth, viewportHeight ) seed
            ( [], seed )

        ( canvasSize, sidebarWidth ) =
            getDims viewportWidth viewportHeight
    in
    { viewportWidth = viewportWidth
    , viewportHeight = viewportHeight
    , sidebarWidth = sidebarWidth
    , canvasSize = canvasSize
    , egg =
        { pos = V2.fromTuple ( 0, 0 )
        , rad = 3
        }
    , hero =
        { state = Shield -- Sword
        , pos = startingPos
        , lastPos = startingPos
        , length = 1
        , angle = 0
        , lastAngle = 0
        , thickness = 1
        , vel = V2.fromTuple ( 0, 0 )
        }
    , enemies = enemies
    , isGameOver = False
    , seed = newSeed
    , timeSinceLastSpawn = 0
    , curTime = 0
    , timeUntilHatch = 2 * 60 * 1000 -- TWO MINUTES TO MIIIIIDNIIIIGHT
    , config =
        { isPaused = True
        , heroLength = 20
        , heroThickness = 5
        , enemySpeed = 1
        , enemySpawnRate = 0.5
        , enemyClusterSize = 3
        }
    , qEnemies = initQueuedEnemies ( viewportWidth, viewportHeight ) seed
    , mousePos = startingPos
    , resources = Resources.init
    , cmds = []
    , kaiju = 0
    }
        ! [ Resources.loadTextures [ "images/crab-spritesheet.png" ]
                |> Cmd.map Resources
          ]


initQueuedEnemies : ( Int, Int ) -> Random.Seed -> List ( Time, Enemy )
initQueuedEnemies ( viewportWidth, viewportHeight ) seed =
    let
        wave1Time =
            0

        wave2Time =
            5000

        wave3Time =
            10000

        wave4Time =
            15000

        wave5Time =
            25000

        twelvth =
            turns (1 / 12)

        top =
            turns 0.25

        bottom =
            turns -0.25
    in
    -- initCluster seed timeToSpawn numEnemies angle
    [ -- wave 1, single quabs from top
      initCluster seed (wave1Time + 0) 1 (top + twelvth)
    , initCluster seed (wave1Time + 1000) 1 top
    , initCluster seed (wave1Time + 2000) 1 (top - twelvth)

    -- wave 2, single quabs from bottom
    , initCluster seed (wave2Time + 0) 1 (bottom - twelvth)
    , initCluster seed (wave2Time + 1000) 1 bottom
    , initCluster seed (wave2Time + 2000) 1 (bottom + twelvth)

    -- wave 3, clusters from the left/right sides
    , initCluster seed (wave3Time + 0) 3 (turns 0.5)
    , initCluster seed (wave3Time + 3000) 3 (turns 0)

    -- wave 4, clusters from all around!
    , List.range 0 12
        |> List.map
            (\i ->
                initCluster seed (wave4Time + (2000 * toFloat i)) 5 (top - (toFloat i * turns (1 / 12)))
            )
        |> List.concat

    -- wave 5, singles spiraling in from all around!
    , List.range 0 48
        |> List.map
            (\i ->
                initCluster seed (wave5Time + (100 * toFloat i)) 1 (top - (toFloat i * turns (1 / 48)))
            )
        |> List.concat
    ]
        |> List.concat
        |> List.sortBy Tuple.first


initCluster : Random.Seed -> Time -> Int -> Float -> List ( Time, Enemy )
initCluster seed timeToSpawn numEnemies angle =
    List.range 1 numEnemies
        |> List.map
            (\i ->
                let
                    timeBetween =
                        600

                    distFromEggToSpawn =
                        quabSpawnDist

                    pos =
                        ( distFromEggToSpawn, angle )
                            |> fromPolar
                            |> V2.fromTuple
                in
                ( timeToSpawn + (timeBetween * toFloat i)
                , { pos = pos
                  , lastPos = pos
                  , rad = 2
                  , state = Alive
                  , seed = seed
                  }
                )
            )


initEnemies : ( Int, Int ) -> Random.Seed -> ( List Enemy, Random.Seed )
initEnemies ( viewportWidth, viewportHeight ) seed =
    -- for debugging mainly
    let
        spacing =
            10

        frac =
            8
    in
    List.range (round (toFloat viewportWidth / frac) // -spacing) (round (toFloat viewportWidth / frac) // spacing)
        |> List.map
            (\w ->
                List.range (round (toFloat viewportHeight / frac) // -spacing) (round (toFloat viewportHeight / frac) // spacing)
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
