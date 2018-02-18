-- WIP


module Config exposing (..)

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, defaultValue)
import Html.Events exposing (onInput)


type Msg
    = NoOp
    | ChangeEnemySpeedo Float


testConfig =
    Floaty "Enemy Speed" 1.23 ChangeEnemySpeedo


type Config a msg
    = Floaty String Float (Float -> msg)
    | Inty String Int (Int -> msg)
    | Booly String Bool (Bool -> msg)



--= Float String Float (Float -> msg)
--| Int String Int (Int -> msg)
--float : Float -> String -> (Float -> msg) -> Config Float msg
--float num title m =
--    Config title num m
--floatVal : Floaty String Float (Float -> msg)
--floatVal floaty =
--    case config of
--        Floaty _ a _ ->
--            a
--
--        Inty _ a _ ->
--            a
--
--        Booly _ a _ ->
--            a
--input : Config a msg -> Html msg
--input config =
--    case config of
--        Config title a msg ->
--            div [ class "config-item" ]
--                [ label []
--                    [ text title
--                    , input
--                        [ defaultValue (toString a)
--                        , onInput msg
--                        ]
--                        []
--                    ]
--                ]
