module View exposing (view)

import Array
import Color exposing (Color)
import Common exposing (..)
import Ease
import ElementRelativeMouseEvents as Mouse
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable, circle, customFragment, rectangle, ring)
import Html exposing (Html, a, br, dd, div, dl, dt, h1, h2, hr, img, input, label, p, span, table, td, text, tr)
import Html.Attributes exposing (checked, class, defaultValue, href, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Math.Vector2 as V2 exposing (Vec2)
import Random
import Time exposing (Time)
import WebGL


eggBorder =
    0.4


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
view ({ hero, enemies, config, curTime, canvasSize, resources, state, viewportWidth, viewportHeight, isStartBtnHovered, effects, numEggs } as model) =
    div [ class "container" ]
        (case state of
            Start ->
                let
                    ( canvasSize, sidebarWidth ) =
                        getDims viewportWidth viewportHeight
                in
                [ div
                    [ class
                        ("start-container"
                            ++ (if isStartBtnHovered then
                                    " is-hovered"
                                else
                                    ""
                               )
                        )
                    , style [ ( "width", px (canvasSize + sidebarWidth) ), ( "height", px canvasSize ) ]
                    ]
                    [ img [ src "images/start-screen.png", class "start-screen-img" ] []
                    , img [ src "images/start-screen-on.png", class "start-screen-on-img" ] []
                    , div
                        [ class "start-btn"
                        , onMouseEnter MouseOnStartBtn
                        , onMouseLeave MouseOutStartBtn
                        , onClick StartGame
                        ]
                        []
                    , div
                        [ class "credits credits-left"
                        ]
                        [ div [ class "credits-group" ]
                            [ text "Created by"
                            , br [] []
                            , a [ href "https://twitter.com/james_gary", target "_blank" ] [ text "@james_gary" ]
                            ]
                        , div [ class "credits-group" ]
                            [ text "Source Code on"
                            , br [] []
                            , a [ href "https://github.com/jamesgary/protect-the-egg", target "_blank" ] [ text "github.com" ]
                            ]
                        ]
                    , div
                        [ class "credits credits-right"
                        ]
                        [ div [ class "credits-group" ]
                            [ text "Music by"
                            , br [] []
                            , a [ href "http://freemusicarchive.org/music/Pharaos/The_New_Pharaos/03_The_Pharaos_Theme_vbrmp3_1375", target "_blank" ] [ text "Pharaos" ]
                            ]
                        , div [ class "credits-group" ]
                            [ text "Sound"
                            , br [] []
                            , a [ href "https://www.bfxr.net/", target "_blank" ] [ text "Bfxr" ]
                            ]
                        ]
                    ]
                ]

            _ ->
                [ div [ class "game-container" ]
                    [ renderSidebar model
                    , div [ class "canvas-container" ]
                        [ renderCenteredWithAlias
                            { time = curTime
                            , camera = camera
                            , size = ( canvasSize, canvasSize )
                            }
                            (List.concat
                                [ viewEggs numEggs
                                , viewHero model
                                , List.concat (List.map (viewEnemy resources curTime) enemies)
                                , List.concat (List.map (viewEffect curTime) effects)
                                ]
                            )
                        ]
                    ]
                , viewGameOver model
                , viewVictory model
                ]
        )


renderTextEffects : Model -> Html Msg
renderTextEffects ({ enemies } as model) =
    div [ class "text-effects-container" ]
        [ div [ class "text-effect" ] [ text "+1" ]
        ]


renderSidebar : Model -> Html Msg
renderSidebar ({ sidebarWidth, curTime, kaiju, config, numEggs, state } as model) =
    div [ class "sidebar", style [ ( "width", px sidebarWidth ) ] ]
        [ div [ class "pause-btn", onClick TogglePause ]
            [ text
                (case state of
                    Paused ->
                        "Unpause"

                    _ ->
                        "Pause"
                )
            ]
        , table [ class "sidebar-list" ]
            [ tr [ class "group" ]
                [ td [ class "label" ] [ text "Time until hatch:" ]
                , td [ class "val" ] [ text (viewTime (timeUntilHatch - curTime)) ]
                ]
            , tr [ class "group" ]
                [ td [ class "label" ] [ text "Eggs left" ]
                , td [ class "val" ] [ formatNum numEggs |> text ]
                ]
            , tr [ class "group" ]
                [ td [ class "label" ] [ text "Kaiju Meter" ]
                , td [ class "val" ] [ renderKaiju kaiju ]
                ]
            ]
        , hr [] []
        , p [ class "instructions" ] [ text "- Quabs are chomping at the bit for your eggs!" ]
        , p [ class "instructions" ] [ text "- Smash them away with your shell!" ]
        , p [ class "instructions" ] [ text "- The more quabs you smash, the more your Kaiju meter increases!" ]
        , p [ class "instructions" ] [ text "- Click to unleash your Kaiju rage!" ]
        ]


renderKaiju : Float -> Html Msg
renderKaiju kaiju =
    div [ class "kaiju-container" ]
        [ div
            [ class "kaiju"
            , style [ ( "width", (min kaiju 100 |> toString) ++ "%" ) ]
            ]
            []
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
                "O" ++ (seconds |> formatNum)
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


eggPosList =
    [ ( -0.9, -1.5 )
    , ( 0.5, 1.2 )
    , ( 0.0, -1.1 )
    , ( 0.9, 0.3 )
    , ( -0.3, 1.3 )
    , ( 0.9, -1.2 )
    , ( 1.4, -0.4 )
    , ( -0.8, 0.3 )
    , ( -1.3, -0.4 )
    , ( 0.2, -0.5 )
    , ( -0.3, -0.3 )
    , ( 0.1, 0.2 )
    ]
        |> List.map V2.fromTuple
        |> List.map (V2.scale 3)


eggSize =
    2


viewEggs : Int -> List Renderable
viewEggs numEggs =
    eggPosList
        |> List.take numEggs
        |> List.map
            (\pos ->
                [ viewOval Color.black pos (eggSize + eggBorder) 0.8
                , viewOval Color.white pos eggSize 0.8
                ]
            )
        |> List.concat


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


viewOval : Color -> Vec2 -> Float -> Float -> Renderable
viewOval color pos rad skew =
    let
        ( x, y ) =
            V2.toTuple pos

        hRad =
            rad * skew

        vRad =
            rad / skew
    in
    Render.shape circle
        { color = color
        , position = ( x - hRad, y - vRad )
        , size = ( hRad * 2, vRad * 2 )
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


viewHero : Model -> List Renderable
viewHero model =
    case model.hero.state of
        Shield ->
            viewShield model

        Sword ->
            viewSword model


viewShield : Model -> List Renderable
viewShield { hero, config, resources, mousePos } =
    let
        { state, pos, lastPos, angle, lastAngle, length, thickness } =
            hero

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
                |> V2.sub pos

        rightEye =
            fromPolar ( -eyeRad, angle )
                |> V2.fromTuple
                |> V2.sub pos

        ( leftEyePupil, rightEyePupil ) =
            if V2.distance pos mousePos > 3 then
                ( leftEye
                    |> V2.add
                        (leftEye
                            |> V2.direction mousePos
                            |> V2.scale 1
                        )
                , rightEye
                    |> V2.add
                        (rightEye
                            |> V2.direction mousePos
                            |> V2.scale 1
                        )
                )
            else
                ( leftEye
                    |> V2.add (V2.fromTuple (fromPolar ( 1, angle + turns 0.25 )))
                , rightEye
                    |> V2.add (V2.fromTuple (fromPolar ( 1, angle + turns 0.25 )))
                )

        ( x, y ) =
            V2.toTuple pos
    in
    List.concat
        [ [ Render.spriteWithOptions
                { texture = Resources.getTexture "images/durdle.png" resources
                , position = ( x, y, 0 )
                , size = ( tl, tt )
                , tiling = ( 1, 1 )
                , rotation = angle
                , pivot = ( 0.5, 0.5 )
                }
          ]
        , -- eye fills
          List.map
            (viewShape colors.eyeFill)
            [ Circle
                { pos = leftEye
                , rad = eyeRad
                }
            , Circle
                { pos = rightEye
                , rad = eyeRad
                }
            ]
        , -- eye pupils
          List.map
            (viewShape colors.eyePupil)
            [ Circle
                { pos = leftEyePupil
                , rad = eyePupilRad
                }
            , Circle
                { pos = rightEyePupil
                , rad = eyePupilRad
                }
            ]
        ]


viewSword : Model -> List Renderable
viewSword { hero, config, resources, mousePos } =
    let
        { state, pos, lastPos, angle, lastAngle, length, thickness } =
            hero

        tl =
            trueLength config hero

        tt =
            trueThickness config hero

        ( x, y ) =
            V2.toTuple pos
    in
    List.concat
        [ [ Render.spriteWithOptions
                { texture = Resources.getTexture "images/kaiju.png" resources
                , position = ( x, y, 0 )
                , size = ( tt, tl )
                , tiling = ( 1, 1 )
                , rotation = angle - turns 0.25
                , pivot = ( 0.5, 0.5 )
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


quabFrames =
    [ ( 0, False )
    , ( 2, False )
    , ( 3, False )
    , ( 2, False )
    , ( 1, True )
    , ( 2, True )
    , ( 3, True )
    , ( 2, True )
    ]
        |> Array.fromList


viewEnemy : Resources -> Time -> Enemy -> List Renderable
viewEnemy resources curTime { pos, rad, state, seed } =
    case state of
        Alive ->
            [ let
                ( x, y ) =
                    pos |> V2.toTuple

                ( frameToShow, isFlipped ) =
                    quabFrames
                        |> Array.get ((curTime / 140 |> round) % Array.length quabFrames)
                        |> Maybe.withDefault ( 0, True )
              in
              Render.manuallyManagedAnimatedSpriteWithOptions
                { position = ( x, y, 0 )
                , size =
                    if isFlipped then
                        ( -11, 11 )
                    else
                        ( 11, 11 )
                , texture = Resources.getTexture "images/quab-spritesheet.png" resources
                , bottomLeft = ( 0, 0 )
                , topRight = ( 1, 1 )
                , duration = 300
                , numberOfFrames = 4
                , rotation = pos |> V2.toTuple |> toPolar |> Tuple.second |> (+) (turns 0.75)
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


viewEffect : Time -> Effect -> List Renderable
viewEffect curTime ({ expTime, kind, pos, seed } as effect) =
    case kind of
        Splash ->
            let
                age =
                    curTime - (expTime - splashLongevity)

                progress =
                    age / splashLongevity

                waterRad =
                    5 + 1.5 * progress

                opacity =
                    0.9 - (0.9 * progress)

                -- List (Float, Float, Float)
                -- [(angle, sizeMod, speed)]
                ( particles, _ ) =
                    Random.step
                        (Random.list 10
                            (Random.map3 (,,)
                                (Random.float 0 (turns 1))
                                (Random.float 0.1 0.6)
                                (Random.float 12 20)
                            )
                        )
                        seed

                color =
                    Color.rgba 255 255 255 opacity
            in
            [ [ viewTransparentCircle color pos waterRad ]
            , particles
                |> List.map
                    (\( angle, sizeMod, speed ) ->
                        fromPolar ( speed * Ease.outCubic progress, angle )
                            |> (\( x, y ) ->
                                    viewTransparentCircle
                                        color
                                        (V2.add pos (V2.fromTuple ( x, y )))
                                        (sizeMod * waterRad)
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


viewGameOver : Model -> Html Msg
viewGameOver { state } =
    case state of
        GameOver ->
            div [ class "game-over-container" ]
                [ div [ class "game-over" ]
                    [ h1 [] [ text "GAME OVER!" ]
                    , div
                        [ class "try-again-btn", onClick TryAgain ]
                        [ text "Try Again" ]
                    ]
                ]

        _ ->
            text ""


viewVictory : Model -> Html Msg
viewVictory ({ state } as model) =
    case state of
        Victory ->
            div [ class "victory-container" ]
                [ div
                    [ class "victory" ]
                    [ p []
                        [ text "You protected your eggs!"
                        , br [] []
                        , text "Your lineage will live on forever."
                        , br [] []
                        , text "This calls for a shell-ebration!"
                        , br [] []
                        ]
                    , div [ class "play-again-btn", onClick TryAgain ] [ text "Play Again" ]
                    ]
                ]

        _ ->
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
