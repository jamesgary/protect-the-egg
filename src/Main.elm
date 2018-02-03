module Main exposing (..)

import AnimationFrame
import Common exposing (..)
import Game.TwoD.Camera exposing (viewportToGameCoordinates)
import Html
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
            initEnemies seed
    in
    ( { windowWidth = windowWidth
      , windowHeight = windowHeight
      , egg =
            { pos =
                { x = 0
                , y = 0
                }
            , rad = 10
            }
      , hero =
            { pos = { x = 300, y = 100 }
            , rad = 7
            , angle = 0
            }
      , enemies = enemies
      , isGameOver = False
      , seed = newSeed
      }
    , Cmd.none
    )


initEnemies : Random.Seed -> ( List Enemy, Random.Seed )
initEnemies seed =
    Random.step (Random.list 10 enemyGenerator) seed


enemyStartingDistFromEgg =
    100


enemyGenerator : Random.Generator Enemy
enemyGenerator =
    Random.float 0 (turns 1)
        |> Random.map
            (\angle ->
                fromPolar ( enemyStartingDistFromEgg, angle )
                    |> (\( x, y ) ->
                            { pos = { x = x, y = y }
                            , rad = 2
                            }
                       )
            )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ windowWidth, windowHeight } as model) =
    case msg of
        MouseMove { x, y } ->
            viewportToGameCoordinates
                camera
                ( cameraWidth, cameraHeight )
                ( round <| toFloat x - 0.5 * (toFloat windowWidth - cameraWidth)
                , round <| toFloat y - 0.5 * (toFloat windowHeight - cameraHeight)
                )
                |> (\( gameX, gameY ) ->
                        moveHero (Pos gameX gameY) model.egg.pos model ! []
                   )

        Tick timeDelta ->
            tick timeDelta model ! []


moveHero : Pos -> Pos -> Model -> Model
moveHero mousePos eggPos ({ hero } as model) =
    { model
        | hero =
            { hero
                | pos = mousePos
                , angle =
                    toPolar ( eggPos.x - hero.pos.x, eggPos.y - hero.pos.y )
                        |> (\( _, angle ) -> angle + turns 0.25)
            }
    }


tick : Time -> Model -> Model
tick timeDelta ({ egg, enemies, hero } as model) =
    let
        movedEnemies =
            List.map (moveEnemyCloserToEgg timeDelta egg) enemies
                |> List.filterMap (doesCollideWithHero hero)

        isGameOver =
            List.any (doesCollideWithEgg egg) movedEnemies
    in
    { model
        | enemies = movedEnemies
        , isGameOver = isGameOver
    }


doesCollideWithEgg : Egg -> Enemy -> Bool
doesCollideWithEgg egg enemy =
    let
        dist_ =
            dist egg.pos enemy.pos
    in
    dist_ < (egg.rad + enemy.rad)


doesCollideWithHero : Hero -> Enemy -> Maybe Enemy
doesCollideWithHero hero enemy =
    let
        dist_ =
            dist hero.pos enemy.pos
    in
    if dist_ < (hero.rad + enemy.rad) then
        Nothing
    else
        Just enemy


dist : Pos -> Pos -> Float
dist pos1 pos2 =
    sqrt ((pos1.x - pos2.x) ^ 2 + (pos1.y - pos2.y) ^ 2)


moveEnemyCloserToEgg : Time -> Egg -> Enemy -> Enemy
moveEnemyCloserToEgg timeDelta egg enemy =
    toPolar ( egg.pos.x - enemy.pos.x, egg.pos.y - enemy.pos.y )
        |> (\( _, angle ) -> fromPolar ( timeDelta * enemySpeed, angle ))
        |> (\( x, y ) -> { enemy | pos = { x = enemy.pos.x + x, y = enemy.pos.y + y } })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions ({ isGameOver } as model) =
    Sub.batch
        [ Mouse.moves MouseMove
        , if isGameOver then
            Sub.none
          else
            AnimationFrame.diffs Tick
        ]
