module Main exposing (..)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)
import Mouse


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , egg : ( Int, Int )
    , hero : ( Int, Int )
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
    }


init : Flag -> ( Model, Cmd Msg )
init { windowWidth, windowHeight } =
    ( { egg = ( windowWidth // 2, windowHeight // 2 )
      , windowWidth = windowWidth
      , windowHeight = windowHeight
      , hero = ( 300, 100 )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MouseMove Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove { x, y } ->
            { model | hero = ( x, y ) } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves MouseMove



-- VIEW


view : Model -> Html Msg
view ({ egg, hero } as model) =
    div [ class "container" ]
        [ viewEgg egg
        , viewHero hero
        ]


viewEgg : ( Int, Int ) -> Html Msg
viewEgg (( x, y ) as egg) =
    div
        [ class "egg sprite"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


viewHero : ( Int, Int ) -> Html Msg
viewHero (( x, y ) as hero) =
    div
        [ class "hero sprite"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


px : Int -> String
px num =
    toString num ++ "px"
