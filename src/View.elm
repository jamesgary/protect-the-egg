module View exposing (view)

import Color exposing (Color)
import Common exposing (..)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, circle, rectangle)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, style)


eggBorder =
    0.8


view : Model -> Html Msg
view ({ egg, hero, enemies, isGameOver } as model) =
    div [ class "container" ]
        [ Game.renderCentered
            { time = 0
            , camera = camera
            , size = ( cameraWidth, cameraHeight )
            }
            (List.concat
                [ viewEgg egg
                , viewHero hero
                , List.concat (List.map viewEnemy enemies)
                ]
            )
        , viewGameOver isGameOver
        ]


viewEgg : Egg -> List Renderable
viewEgg { pos, rad } =
    [ viewCircle Color.black pos (rad + eggBorder)
    , viewCircle Color.white pos rad
    ]


viewCircle : Color -> Pos -> Float -> Renderable
viewCircle color pos rad =
    Render.shape circle
        { color = color
        , position = ( pos.x - rad, pos.y - rad )
        , size = ( rad * 2, rad * 2 )
        }


heroBorder =
    0.5


viewHero : Hero -> List Renderable
viewHero { pos, width, height, angle } =
    let
        ( x, y ) =
            -- hold the sword/shield from the bottom middle
            fromPolar ( height, angle )
                |> (\( offsetX, offsetY ) ->
                        ( pos.x - 0.5 * offsetY
                        , pos.y + 0.5 * offsetX
                        )
                   )
    in
    [ Render.shapeWithOptions rectangle
        { color = Color.black
        , position = ( x, y, 0 )
        , size =
            ( width + (heroBorder * 2)
            , height + (heroBorder * 2)
            )
        , rotation = angle
        , pivot = ( 0.5, 0.5 )
        }
    , Render.shapeWithOptions rectangle
        { color = Color.rgb 255 55 186
        , position = ( x, y, 0 )
        , size = ( width, height )
        , rotation = angle
        , pivot = ( 0.5, 0.5 )
        }
    ]


viewEnemy : Enemy -> List Renderable
viewEnemy { pos, rad } =
    [ viewCircle Color.black pos (rad + eggBorder)
    , viewCircle Color.red pos rad
    ]


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
