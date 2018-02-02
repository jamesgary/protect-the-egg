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
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
    }


type alias Egg =
    ( Float, Float )


type alias Hero =
    ( Float, Float )


type alias Enemy =
    ( Float, Float )


init : Flag -> ( Model, Cmd Msg )
init { windowWidth, windowHeight } =
    ( { egg = ( toFloat windowWidth / 2, toFloat windowHeight / 2 )
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , hero = ( 300, 100 )
      , enemies = [ ( 30, 30 ) ]
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
            { model | hero = ( toFloat x, toFloat y ) } ! []

        Tick timeDelta ->
            tick timeDelta model ! []


tick : Time -> Model -> Model
tick timeDelta ({ egg, enemies } as model) =
    { model | enemies = List.map (moveEnemyCloserToEgg timeDelta egg) enemies }


moveEnemyCloserToEgg : Time -> Egg -> Enemy -> Enemy
moveEnemyCloserToEgg timeDelta ( eggX, eggY ) ( enemyX, enemyY ) =
    toPolar ( eggX - enemyX, eggY - enemyY )
        |> (\( _, angle ) -> fromPolar ( timeDelta * enemySpeed, angle ))
        |> (\( x, y ) -> ( enemyX + x, enemyY + y ))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , AnimationFrame.diffs Tick
        ]



-- VIEW


view : Model -> Html Msg
view ({ egg, hero, enemies } as model) =
    div [ class "container" ]
        ([ viewEgg egg
         , viewHero hero
         ]
            ++ List.map viewEnemy enemies
        )


viewEgg : Egg -> Html Msg
viewEgg (( x, y ) as egg) =
    div
        [ class "egg sprite"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


viewHero : Hero -> Html Msg
viewHero (( x, y ) as hero) =
    div
        [ class "hero sprite"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


viewEnemy : Enemy -> Html Msg
viewEnemy (( x, y ) as hero) =
    div
        [ class "enemy sprite"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


px : number -> String
px num =
    toString num ++ "px"
