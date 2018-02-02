module Main exposing (..)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)


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
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        [ class "egg"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


viewHero : ( Int, Int ) -> Html Msg
viewHero (( x, y ) as hero) =
    div
        [ class "hero"
        , style
            [ ( "transform", "translate(" ++ px x ++ "," ++ px y ++ ")" )
            ]
        ]
        []


px : Int -> String
px num =
    toString num ++ "px"
