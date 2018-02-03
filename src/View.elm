module View exposing (view)

import Common exposing (..)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)


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
viewHero { pos, rad, angle } =
    div
        [ class "hero sprite"
        , style
            [ ( "transform"
              , "translate("
                    ++ px pos.x
                    ++ ","
                    ++ px pos.y
                    ++ ")"
                    ++ " rotate("
                    ++ toString angle
                    ++ "rad)"
              )
            , ( "width", px (2 * rad) )
            , ( "height", px rad )
            , ( "top", px (-0.5 * rad) )
            , ( "left", px (-1 * rad) )
            , ( "border-bottom-left-radius", px (2 * rad) )
            , ( "border-bottom-right-radius", px (2 * rad) )
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
