module View exposing (view)

import Array
import Color exposing (Color)
import Common exposing (..)
import Config
import Ease
import ElementRelativeMouseEvents as Mouse
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, circle, customFragment, rectangle, ring)
import Html exposing (Html, br, dd, div, dl, dt, h1, h2, input, label, p, span, table, td, text, tr)
import Html.Attributes exposing (checked, class, defaultValue, style, type_)
import Html.Events exposing (onClick, onInput)
import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)
import WebGL


eggBorder =
    0.8


colors =
    { sand = Color.rgb 248 233 156
    , shellBorder = Color.rgb 50 120 30 --21 93 1
    , shellInner = Color.rgb 127 255 89 --(57, 220, 10)
    , durdleSkin = Color.rgb 255 255 0 --219 255 70
    , durdleSkinBorder = Color.rgb 180 160 0 --220 200 0 --219 255 70
    , eyeFill = Color.rgb 255 255 255
    , eyePupil = Color.rgb 0 0 0
    , beachBg = Color.rgb 248 233 156
    , oceanBlue = Color.rgb 50 50 255
    }


view : Model -> Html Msg
view ({ egg, hero, enemies, config, curTime, isGameOver, canvasSize, resources } as model) =
    div [ class "container" ]
        [ div [ class "game-container" ]
            [ renderSidebar model
            , div [ class "canvas-container" ]
                [ renderCenteredWithAlias
                    { time = curTime
                    , camera = camera
                    , size = ( canvasSize, canvasSize )
                    }
                    (List.concat
                        [ --viewBg model
                          viewEgg egg
                        , viewHero config hero
                        , List.concat (List.map (viewEnemy resources curTime) enemies)
                        ]
                    )

                --, renderTextEffects model
                ]
            ]
        , viewGameOver isGameOver

        --, viewConfig config
        ]


renderTextEffects : Model -> Html Msg
renderTextEffects ({ enemies } as model) =
    div [ class "text-effects-container" ]
        [ div [ class "text-effect" ] [ text "+1" ]
        ]


renderSidebar : Model -> Html Msg
renderSidebar model =
    div [ class "sidebar", style [ ( "width", px model.sidebarWidth ) ] ]
        [ div [ class "pause-btn", onClick TogglePause ]
            [ text
                (if model.config.isPaused then
                    "Unpause"
                 else
                    "Pause"
                )
            ]
        , table [ class "sidebar-list" ]
            [ tr [ class "group" ]
                [ td [ class "label" ] [ text "Time until hatch:" ]
                , td [ class "val" ] [ text (viewTime (model.timeUntilHatch - model.curTime)) ]
                ]
            , tr [ class "group" ]
                [ td [ class "label" ] [ text "Score" ]
                , td [ class "val" ] [ formatNum 1024 |> text ]
                ]
            , tr [ class "group" ]
                [ td [ class "label" ] [ text "Eggs left" ]
                , td [ class "val" ] [ formatNum 12 |> text ]
                ]
            , tr [ class "group" ]
                [ td [ class "label" ] [ text "Kaiju Meter" ]
                , td [ class "val" ] [ text "82/1OO" ]
                ]
            ]
        ]


viewTime : Time -> String
viewTime time =
    let
        minutes =
            time |> Time.inMinutes |> floor

        seconds =
            (time |> Time.inSeconds) - (toFloat minutes * 60) |> floor

        minutesStr =
            minutes |> formatNum

        secondsStr =
            if seconds < 10 then
                "0" ++ (seconds |> formatNum)
            else
                seconds |> formatNum
    in
    minutesStr ++ ":" ++ secondsStr


renderCenteredWithAlias : Game.RenderConfig -> List Renderable -> Html Msg
renderCenteredWithAlias { time, size, camera } renderables =
    let
        ( w, h ) =
            size

        ( wf, hf ) =
            ( toFloat w, toFloat h )
    in
    WebGL.toHtmlWith
        [ WebGL.alpha True
        , WebGL.depth 1
        , WebGL.antialias
        ]
        [ Html.Attributes.width w
        , Html.Attributes.height h
        , Mouse.onMouseMove MouseMove
        , Mouse.onClick MouseClick
        ]
        (List.map (Render.toWebGl time camera ( wf, hf )) renderables)


viewConfig : Config -> Html Msg
viewConfig { isPaused, heroLength, heroThickness, enemySpeed, enemySpawnRate, enemyClusterSize } =
    div [ class "config" ]
        [ h2 [] [ text "Config" ]
        , configCheckbox "Pause" isPaused TogglePause
        , configInput "Hero Length" heroLength ChangeHeroLength
        , configInput "Hero Thickness" heroThickness ChangeHeroThickness
        , configInput "Enemy Speed" enemySpeed ChangeEnemySpeed
        , configInput "Enemy Spawn/s Rate" enemySpawnRate ChangeEnemySpawnRate
        , configInput "Enemy Cluster Size" enemyClusterSize ChangeEnemyClusterSize
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


viewBg : Model -> List Renderable
viewBg { viewportWidth, viewportHeight } =
    [ viewShape colors.oceanBlue
        (Rect
            { pos = V2.fromTuple ( 0, 0 )
            , width = toFloat viewportWidth
            , height = toFloat viewportHeight
            , angle = 0
            }
        )
    , viewShape colors.beachBg
        (Circle
            { pos = V2.fromTuple ( 0, 0 )
            , rad = 0.07 * toFloat viewportHeight -- wat
            }
        )
    ]


viewEgg : Egg -> List Renderable
viewEgg { pos, rad } =
    [ viewCircle Color.black pos (rad + eggBorder)
    , viewCircle Color.white pos rad
    ]


viewRing : Color -> Vec2 -> Float -> Renderable
viewRing color pos rad =
    let
        ( x, y ) =
            V2.toTuple pos
    in
    Render.shape ring
        { color = color
        , position = ( x - rad, y - rad )
        , size = ( rad * 2, rad * 2 )
        }


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


borderWidth =
    1


viewHero : Config -> Hero -> List Renderable
viewHero config ({ state, pos, lastPos, angle, lastAngle, length, thickness } as hero) =
    let
        tl =
            trueLength config hero

        tt =
            trueThickness config hero

        sideOffset =
            fromPolar ( tl / 2, angle )
                |> V2.fromTuple

        halfwayDownOffset =
            fromPolar ( tt / 4, angle + turns 0.25 )
                |> V2.fromTuple

        almostHalfwayDownOffset =
            fromPolar ( (tt - (2 * borderWidth)) / 4, angle + turns 0.25 )
                |> V2.fromTuple

        eyeRad =
            tt / 3

        eyePupilRad =
            eyeRad / 2

        leftEye =
            fromPolar ( eyeRad, angle )
                |> V2.fromTuple

        rightEye =
            fromPolar ( -eyeRad, angle )
                |> V2.fromTuple

        leftEyePupil =
            leftEye
                |> V2.add (V2.fromTuple (fromPolar ( 1, angle + turns -0.25 )))

        rightEyePupil =
            rightEye
                |> V2.add (V2.fromTuple (fromPolar ( 1, angle + turns -0.25 )))

        ( x, y ) =
            V2.toTuple pos
    in
    List.concat
        [ -- shell border
          List.map
            (viewShape colors.shellBorder)
            [ Rect
                { pos = pos
                , width = tl
                , height = tt
                , angle = angle
                }
            , Circle
                { pos = V2.add pos sideOffset
                , rad = tt / 2
                }
            , Circle
                { pos = V2.sub pos sideOffset
                , rad = tt / 2
                }
            ]
        , -- shell fill
          List.map
            (viewShape colors.shellInner)
            [ Rect
                { pos = pos
                , width = tl - borderWidth
                , height = tt - borderWidth
                , angle = angle
                }
            , Circle
                { pos = V2.add pos sideOffset
                , rad = (tt / 2) - (0.5 * borderWidth)
                }
            , Circle
                { pos = V2.sub pos sideOffset
                , rad = (tt / 2) - (0.5 * borderWidth)
                }
            ]
        , -- inner shell border
          List.map
            (viewShape colors.shellBorder)
            [ Rect
                { pos = V2.sub pos almostHalfwayDownOffset
                , width = tl + borderWidth
                , height = (tt + (2 * borderWidth)) / 2
                , angle = angle
                }
            ]
        , -- durdle body border
          List.map
            (viewShape colors.durdleSkinBorder)
            [ Rect
                { pos = V2.sub pos halfwayDownOffset
                , width = tl
                , height = tt / 2
                , angle = angle
                }
            ]
        , -- durdle body fill
          List.map
            (viewShape colors.durdleSkin)
            [ Rect
                { pos = V2.sub pos almostHalfwayDownOffset
                , width = tl
                , height = tt / 2
                , angle = angle
                }
            ]
        , -- eye fills
          List.map
            (viewShape colors.eyeFill)
            [ Circle
                { pos = V2.sub pos leftEye
                , rad = eyeRad
                }
            , Circle
                { pos = V2.sub pos rightEye
                , rad = eyeRad
                }
            ]
        , -- eye pupils
          List.map
            (viewShape colors.eyePupil)
            [ Circle
                { pos = V2.sub pos leftEyePupil
                , rad = eyePupilRad
                }
            , Circle
                { pos = V2.sub pos rightEyePupil
                , rad = eyePupilRad
                }
            ]
        ]


heroColor =
    Color.rgb 20 130 80


blurColor =
    Color.rgb 255 255 255


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


viewEnemy : Resources -> Time -> Enemy -> List Renderable
viewEnemy resources curTime { pos, rad, state, seed } =
    case state of
        Alive ->
            [ --viewCircle Color.black pos (rad + eggBorder),
              --viewCircle (Color.rgba 255 0 0 0.02) pos rad
              let
                ( x, y ) =
                    pos |> V2.toTuple

                ( frameToShow, isFlipped ) =
                    [ ( 1, False )
                    , ( 2, False )
                    , ( 3, False )
                    , ( 2, False )
                    , ( 0, True )
                    , ( 2, True )
                    , ( 3, True )
                    , ( 2, True )
                    ]
                        |> Array.fromList
                        |> Array.get ((curTime / 140 |> round) % 8)
                        |> Maybe.withDefault ( 0, True )
              in
              Render.manuallyManagedAnimatedSpriteWithOptions
                { position = ( x, y, 0 )
                , size =
                    if isFlipped then
                        ( -11, 11 )
                    else
                        ( 11, 11 )
                , texture = Resources.getTexture "images/crab-spritesheet.png" resources
                , bottomLeft = ( 0, 0 )
                , topRight = ( 1, 1 )
                , duration = 300
                , numberOfFrames = 4
                , rotation = pos |> V2.toTuple |> toPolar |> Tuple.second |> (+) (turns 0.25)
                , pivot = ( 0.5, 0.5 )
                , currentFrame = frameToShow
                }
            ]

        Bouncing angle ->
            [ viewCircle (Color.rgba 200 0 0 0.02) pos rad
            , viewCircle (Color.rgba 255 200 200 0.02) pos (rad * 0.9)
            ]

        Exploding expTime ->
            let
                age =
                    curTime - (expTime - explosionLongevity)

                progress =
                    age / explosionLongevity

                smokeRad =
                    (rad * 2) + (2 * rad * progress)

                opacity =
                    0.8 - (0.8 * progress)

                -- List (Float, Float, Float)
                -- [(angle, size, speed)]
                ( particles, _ ) =
                    Random.step
                        (Random.list 10
                            (Random.map3 (,,)
                                (Random.float 0 (turns 1))
                                (Random.float 1 2)
                                (Random.float 10 15)
                            )
                        )
                        seed

                color =
                    Color.rgba 255 200 200 opacity
            in
            [ [ viewTransparentCircle color pos smokeRad ]
            , particles
                |> List.map
                    (\( angle, size, speed ) ->
                        fromPolar ( speed * Ease.outCubic progress, angle )
                            |> (\( x, y ) ->
                                    viewTransparentCircle
                                        color
                                        (V2.add pos (V2.fromTuple ( x, y )))
                                        (0.5 * smokeRad)
                               )
                    )
            ]
                |> List.concat


viewTransparentCircle : Color -> Vec2 -> Float -> Renderable
viewTransparentCircle color pos rad =
    let
        pos3d =
            V2.toTuple pos |> (\( x, y ) -> ( x, y, 0 ))

        { red, green, blue, alpha } =
            Color.toRgb color

        makeUniforms { cameraProj, transform } =
            { cameraProj = cameraProj
            , transform = transform
            , radius = 0.5
            , red = toFloat red / 255
            , green = toFloat green / 255
            , blue = toFloat blue / 255
            , alpha = alpha
            }

        size =
            2 * rad
    in
    customFragment makeUniforms
        { fragmentShader = frag
        , position = pos3d
        , size = ( size, size )
        , rotation = 0
        , pivot = ( 0.5, 0.5 )
        }



--testRenderable : Renderable
--testRenderable =
--    veryCustom
--        (\{ camera, screenSize, time } ->
--            WebGL.entity
--                myVert
--                myFrag
--                Shapes.unitSquare
--                { u_crazyFrog = frogTexture
--                , transform = Shaders.makeTransform ( x, y, z ) 0 ( 2, 4 ) ( 0, 0 )
--                , cameraProj = Camera.view camera screenSize
--                }
--        )


frag =
    [glsl|

precision mediump float;

varying vec2 vcoord;
uniform float radius;
uniform float red;
uniform float green;
uniform float blue;
uniform float alpha;

void main () {
  float dist = length(vec2(0.5, 0.5) - vcoord);

  float circleAlpha = 1.0 - smoothstep(radius - 0.09, radius, dist);
  vec4 color = vec4(red, green, blue, circleAlpha * alpha);

  gl_FragColor = color;
}
|]


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


formatNum : number -> String
formatNum num =
    -- monospace numbers zeroes have dots in them,
    -- so replace zeroes with letter Os
    num
        |> toString
        |> String.map
            (\c ->
                if c == '0' then
                    'O'
                else
                    c
            )
