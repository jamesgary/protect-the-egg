module View exposing (view)

import Color exposing (Color)
import Common exposing (..)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, circle, rectangle)
import Html exposing (Html, div, h1, h2, input, label, text)
import Html.Attributes exposing (checked, class, defaultValue, style, type_)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as V2 exposing (Vec2)


eggBorder =
    0.8


view : Model -> Html Msg
view ({ egg, hero, enemies, config, isGameOver, cameraWidth, cameraHeight } as model) =
    div [ class "container" ]
        [ Game.renderCentered
            { time = 0
            , camera = camera
            , size =
                let
                    w =
                        toFloat cameraWidth

                    h =
                        toFloat cameraHeight
                in
                (if w / h > 16 / 9 then
                    -- too wide!
                    ( h * (16 / 9), h )
                 else
                    -- too tall!
                    ( w, w * (9 / 16) )
                )
                    |> (\( w, h ) -> ( round w, round h ))
            }
            (List.concat
                [ viewEgg egg
                , viewHero config hero
                , List.concat (List.map viewEnemy enemies)
                ]
            )
        , viewGameOver isGameOver
        , viewConfig config
        ]


viewConfig : Config -> Html Msg
viewConfig { isPaused, heroLength, heroThickness } =
    div [ class "config" ]
        [ h2 [] [ text "Config" ]
        , configCheckbox "Pause" isPaused TogglePause
        , configInput "Hero Length" heroLength ChangeHeroLength
        , configInput "Hero Thickness" heroThickness ChangeHeroThickness
        ]


configCheckbox : String -> Bool -> Msg -> Html Msg
configCheckbox title isChecked msg =
    div [ class "config-item" ]
        [ label []
            [ text title
            , input
                [ type_ "checkbox"
                , checked isChecked
                , onClick msg
                ]
                []
            ]
        ]


configInput : String -> number -> (String -> Msg) -> Html Msg
configInput title val msg =
    div [ class "config-item" ]
        [ label []
            [ text title
            , input
                [ defaultValue (toString val)
                , onInput msg
                ]
                []
            ]
        ]


viewEgg : Egg -> List Renderable
viewEgg { pos, rad } =
    [ viewCircle Color.black pos (rad + eggBorder)
    , viewCircle Color.white pos rad
    ]


viewCircle : Color -> Vec2 -> Float -> Renderable
viewCircle color pos rad =
    let
        ( x, y ) =
            V2.toTuple pos
    in
    Render.shape circle
        { color = color
        , position = ( x - rad, y - rad )
        , size = ( rad * 2, rad * 2 )
        }


heroBorder =
    0.5


viewHero : Config -> Hero -> List Renderable
viewHero config ({ state, pos, lastPos, angle, lastAngle, length, thickness } as hero) =
    let
        tl =
            trueLength config hero

        tt =
            trueThickness config hero

        ( rotOffsetX, rotOffsetY ) =
            fromPolar ( tl / 2, angle )

        ( rotOffsetXLast, rotOffsetYLast ) =
            fromPolar ( tl / 2, lastAngle )

        ( a, b, c, d ) =
            getHeroSweepQuadPoints config hero

        ( x, y ) =
            V2.toTuple pos
    in
    List.concat
        [ List.map
            (viewShape heroColor)
            [ Rect
                { pos = pos
                , width = tl
                , height = tt
                , angle = angle
                }
            , Circle { pos = V2.fromTuple ( x + rotOffsetX, y + rotOffsetY ), rad = tt / 2 }
            , Circle { pos = V2.fromTuple ( x - rotOffsetX, y - rotOffsetY ), rad = tt / 2 }
            ]
        , [] --[ viewShape (Color.rgb 255 200 200) (Circle { pos = a, rad = 2 })

        --, viewShape (Color.rgb 255 100 100) (Circle { pos = b, rad = 2 })
        --, viewShape (Color.rgb 0 255 255) (Circle { pos = c, rad = 2 })
        --, viewShape (Color.rgb 0 0 255) (Circle { pos = d, rad = 2 })
        --]
        ]


heroColor =
    Color.rgb 20 130 80


blurColor =
    Color.rgb 255 255 255



--[ List.map (viewShape (Color.rgb 255 195 246)) last
--, List.map (viewShape (Color.rgb 255 55 186)) cur
--]


viewShape : Color -> Shape -> Renderable
viewShape color shape =
    case shape of
        Rect { pos, width, height, angle } ->
            Render.shapeWithOptions rectangle
                { color = color
                , position = ( V2.getX pos, V2.getY pos, 0 )
                , size = ( width, height )
                , rotation = angle
                , pivot = ( 0.5, 0.5 )
                }

        Circle { pos, rad } ->
            Render.shape circle
                { color = color
                , position = ( V2.getX pos - rad, V2.getY pos - rad )
                , size = ( rad * 2, rad * 2 )
                }



--[{- Render.shapeWithOptions rectangle
--    { color = Color.black
--    , position = ( x, y, 0 )
--    , size =
--        ( width + (heroBorder * 2)
--        , height + (heroBorder * 2)
--        )
--    , rotation = angle
--    , pivot = ( 0.5, 0.5 )
--    }
--    ,
-- -}
-- {- Render.shapeWithOptions rectangle
--    { color = Color.rgb 255 55 186
--    , position = ( x, y, 0 )
--    , size = ( width, height )
--    , rotation = angle
--    , pivot = ( 0.5, 0.5 )
--    }
-- -}
--]


viewEnemy : Enemy -> List Renderable
viewEnemy { pos, rad } =
    [ --viewCircle Color.black pos (rad + eggBorder),
      viewCircle Color.red pos rad
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
