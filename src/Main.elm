module Main exposing (..)

import AnimationFrame
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)
import Mouse
import Time exposing (Time)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


enemySpeed =
    0.05


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , egg : Egg
    , hero : Hero
    , enemies : List Enemy
    , isGameOver : Bool
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
    }


type alias Egg =
    { pos : Pos
    , rad : Float
    }


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Hero =
    { pos : Pos
    , rad : Float
    }


type alias Enemy =
    { pos : Pos
    , rad : Float
    }


init : Flag -> ( Model, Cmd Msg )
init { windowWidth, windowHeight } =
    ( { windowWidth = windowWidth
      , windowHeight = windowHeight
      , egg =
            { pos =
                { x = toFloat windowWidth / 2
                , y = toFloat windowHeight / 2
                }
            , rad = 50
            }
      , hero =
            { pos = { x = 300, y = 100 }
            , rad = 25
            }
      , enemies =
            [ { pos = { x = 30, y = 30 }
              , rad = 10
              }
            ]
      , isGameOver = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MouseMove Mouse.Position
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove { x, y } ->
            moveHero (Pos (toFloat x) (toFloat y)) model ! []

        Tick timeDelta ->
            tick timeDelta model ! []


moveHero : Pos -> Model -> Model
moveHero mousePos ({ hero } as model) =
    let
        newHero =
            { hero | pos = mousePos }
    in
    { model | hero = newHero }


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
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , AnimationFrame.diffs Tick
        ]



-- VIEW


view : Model -> Html Msg
view ({ egg, hero, enemies, isGameOver } as model) =
    div [ class "container" ]
        ([ viewGameOver isGameOver
         , viewEgg egg
         , viewHero hero
         ]
            ++ List.map viewEnemy enemies
        )


viewEgg : Egg -> Html Msg
viewEgg { pos, rad } =
    div
        [ class "egg sprite"
        , style
            [ ( "transform"
              , "translate("
                    ++ px pos.x
                    ++ ","
                    ++ px pos.y
                    ++ ")"
              )
            , ( "width", px (2 * rad) )
            , ( "height", px (2 * rad) )
            , ( "top", px (-1 * rad) )
            , ( "left", px (-1 * rad) )
            ]
        ]
        []


viewHero : Hero -> Html Msg
viewHero { pos, rad } =
    div
        [ class "hero sprite"
        , style
            [ ( "transform"
              , "translate("
                    ++ px pos.x
                    ++ ","
                    ++ px pos.y
                    ++ ")"
              )
            , ( "width", px (2 * rad) )
            , ( "height", px (2 * rad) )
            , ( "top", px (-1 * rad) )
            , ( "left", px (-1 * rad) )
            ]
        ]
        []


viewEnemy : Enemy -> Html Msg
viewEnemy { pos, rad } =
    div
        [ class "enemy sprite"
        , style
            [ ( "transform"
              , "translate("
                    ++ px pos.x
                    ++ ","
                    ++ px pos.y
                    ++ ")"
              )
            , ( "width", px (2 * rad) )
            , ( "height", px (2 * rad) )
            , ( "top", px (-1 * rad) )
            , ( "left", px (-1 * rad) )
            ]
        ]
        []


viewGameOver : Bool -> Html Msg
viewGameOver isGameOver =
    if isGameOver then
        div [ class "game-over-container" ]
            [ h1
                [ class "game-over" ]
                [ text "GAME OVER!" ]
            ]
    else
        text ""


px : number -> String
px num =
    toString num ++ "px"
