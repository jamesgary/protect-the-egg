module Main exposing (..)

import AnimationFrame
import Common exposing (..)
import ElementRelativeMouseEvents as Mouse
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera exposing (viewportToGameCoordinates)
import Html
import Init exposing (init, tryAgain)
import Math
import Math.Vector2 as V2 exposing (Vec2)
import Ports exposing (gameOver, pauseSong, playSong, playWav, victory, windowChanged)
import Random
import Time exposing (Time)
import View exposing (view)


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


quabSpeed =
    0.02


clusterGenerator : Config -> Random.Generator (List Quab)
clusterGenerator { quabClusterSize } =
    Random.float 0 (turns 1)
        |> Random.map
            (\angle ->
                List.range 0 (quabClusterSize - 1)
                    |> List.map
                        (\i ->
                            fromPolar ( quabStartingDistFromNest + (5 * toFloat i), angle )
                                |> (\( x, y ) ->
                                        { pos = V2.fromTuple ( x, y )
                                        , lastPos = V2.fromTuple ( x, y )
                                        , rad = 2
                                        , state = Alive
                                        , seed = Random.initialSeed (round (angle * toFloat Random.maxInt * toFloat i))
                                        , lastAteAt = 0
                                        }
                                   )
                        )
            )


toggleState : Durdle -> Durdle
toggleState ({ state, angle } as durdle) =
    case state of
        Sword ->
            { durdle | state = Shield, angle = angle - turns 0.25 }

        Shield ->
            { durdle | state = Sword, angle = angle + turns 0.25 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ durdle, config, state } as model) =
    case msg of
        MouseClick mousePos ->
            { model
                | durdle =
                    toggleState durdle
                        |> (\durdle -> { durdle | pos = trueMousePos model mousePos })
            }
                ! []

        MouseMove mousePos ->
            { model | mousePos = trueMousePos model mousePos } ! []

        Tick timeDelta ->
            -- max time delta is 30 FPS (1000 / 30 == 33)
            tick (min timeDelta 33) model

        WindowChanged ( width, height ) ->
            let
                ( canvasSize, sidebarWidth ) =
                    getDims width height
            in
            { model
                | viewportWidth = width
                , viewportHeight = height
                , canvasSize = canvasSize
                , sidebarWidth = sidebarWidth
            }
                ! []

        TogglePause ->
            case state of
                Paused ->
                    { model | state = Playing } ! [ playSong () ]

                _ ->
                    { model | state = Paused } ! [ pauseSong () ]

        ChangeDurdleLength inputStr ->
            let
                newConfig =
                    { config
                        | durdleLength =
                            String.toFloat inputStr
                                |> Result.withDefault config.durdleLength
                    }
            in
            { model | config = newConfig } ! []

        ChangeDurdleThickness inputStr ->
            let
                newConfig =
                    { config
                        | durdleThickness =
                            String.toFloat inputStr
                                |> Result.withDefault config.durdleThickness
                    }
            in
            { model | config = newConfig } ! []

        ChangeQuabSpeed inputStr ->
            let
                newConfig =
                    { config
                        | quabSpeed =
                            String.toFloat inputStr
                                |> Result.withDefault config.quabSpeed
                    }
            in
            { model | config = newConfig } ! []

        ChangeQuabSpawnRate inputStr ->
            let
                newConfig =
                    { config
                        | quabSpawnRate =
                            String.toFloat inputStr
                                |> Result.withDefault config.quabSpawnRate
                    }
            in
            { model | config = newConfig } ! []

        ChangeQuabClusterSize inputStr ->
            let
                newConfig =
                    { config
                        | quabClusterSize =
                            String.toInt inputStr
                                |> Result.withDefault config.quabClusterSize
                    }
            in
            { model | config = newConfig } ! []

        Resources msg ->
            { model | resources = Resources.update msg model.resources } ! []

        MouseOnStartBtn ->
            { model | isStartBtnHovered = True } ! []

        MouseOutStartBtn ->
            { model | isStartBtnHovered = False } ! []

        StartGame ->
            { model | state = Playing } ! [ playWav "pharaos.mp3" ]

        TryAgain ->
            tryAgain model ! [ playSong () ]


trueMousePos : Model -> Mouse.Point -> Vec2
trueMousePos { viewportWidth, viewportHeight } { x, y } =
    let
        w =
            toFloat viewportWidth

        h =
            toFloat viewportHeight

        ( innerWidth, innerHeight ) =
            if w - h > 0 then
                -- too wide!
                ( h, h )
            else
                -- too tall!
                ( w, w )
    in
    viewportToGameCoordinates
        camera
        ( innerWidth |> round, innerHeight |> round )
        ( round <| x
        , round <| y
        )
        |> (\( gameX, gameY ) ->
                V2.fromTuple ( gameX, gameY )
           )


hiltPosFromDurdle : Config -> Durdle -> Vec2
hiltPosFromDurdle config durdle =
    case durdle.state of
        Shield ->
            durdle.pos

        Sword ->
            fromPolar ( trueLength config durdle / 2, durdle.angle )
                |> V2.fromTuple
                |> V2.sub durdle.pos


durdlePosFromHilt : Config -> Durdle -> Vec2 -> Vec2
durdlePosFromHilt config durdle hiltPos =
    case durdle.state of
        Shield ->
            hiltPos

        Sword ->
            fromPolar ( trueLength config durdle / 2, durdle.angle )
                |> V2.fromTuple
                |> V2.add hiltPos


stiffness =
    170


damping =
    20


pTolerance =
    0.01


vTolerance =
    0.01


moveHilt : Time -> Vec2 -> Vec2 -> Vec2 -> ( Vec2, Vec2 )
moveHilt timeDelta oldPos oldVel targetPos =
    -- From https://github.com/mdgriffith/elm-style-animation/blob/
    -- 86f81b0f5a28289894fe61c14fa2c34c0bf895ec/src/Animation/Model.elm#L1591-L1623
    let
        fspring =
            V2.sub targetPos oldPos
                |> V2.scale stiffness

        fdamper =
            oldVel
                |> V2.scale -damping

        a =
            V2.add fspring fdamper

        newVelocity =
            V2.add oldVel (V2.scale (timeDelta / 1000) a)

        newPos =
            V2.add oldPos (V2.scale (timeDelta / 1000) newVelocity)

        dx =
            V2.distance targetPos newPos
    in
    if dx < pTolerance && V2.length newVelocity < vTolerance then
        ( targetPos, V2.fromTuple ( 0, 0 ) )
    else
        ( newPos, newVelocity )


moveDurdle : Time -> Model -> Model
moveDurdle timeDelta ({ config, durdle, mousePos } as model) =
    let
        -- hilt is the mouse-controllable point
        hiltPos =
            hiltPosFromDurdle config durdle

        ( newHiltPos, newVel ) =
            moveHilt timeDelta hiltPos durdle.vel mousePos

        aroundNestPos =
            newHiltPos
                |> V2.distance nestPos
                |> (\d ->
                        if d < nestRad then
                            V2.direction newHiltPos nestPos
                                |> V2.normalize
                                |> V2.scale nestRad
                        else if d > beachRad then
                            V2.direction newHiltPos nestPos
                                |> V2.normalize
                                |> V2.scale beachRad
                        else
                            newHiltPos
                   )

        angle =
            case durdle.state of
                Shield ->
                    V2.sub nestPos aroundNestPos
                        |> V2.toTuple
                        |> toPolar
                        |> (\( _, angle ) ->
                                angle + turns 0.25
                           )

                Sword ->
                    V2.sub nestPos aroundNestPos
                        |> V2.toTuple
                        |> toPolar
                        |> (\( _, angle ) ->
                                angle + turns 0.5
                           )

        newDurdle =
            { durdle
                | pos = durdlePosFromHilt config { durdle | angle = angle } aroundNestPos
                , lastPos = durdle.pos
                , vel = newVel
                , angle = angle
                , lastAngle = durdle.angle
            }
    in
    { model | durdle = newDurdle }


tick : Time -> Model -> ( Model, Cmd Msg )
tick timeDelta ({ config, quabs, durdle, seed, mousePos } as model) =
    model
        |> updateCurTime timeDelta
        |> exhaustKaiju timeDelta
        |> moveDurdle timeDelta
        |> spawnQuabs
        |> moveQuabs timeDelta
        |> collideDurdleAndQuabs
        |> removeDeadQuabs
        |> eatEggs
        |> checkGameOver
        |> checkVictory
        |> cmdify


exhaustKaiju : Time -> Model -> Model
exhaustKaiju timeDelta ({ kaiju, durdle } as model) =
    case durdle.state of
        Shield ->
            model

        Sword ->
            let
                kaijuRemaining =
                    kaiju - (timeDelta * 0.01)
            in
            { model
                | kaiju = kaijuRemaining
                , durdle =
                    { durdle
                        | state =
                            if kaijuRemaining > 0 then
                                Sword
                            else
                                Shield
                    }
            }


cmdify : Model -> ( Model, Cmd Msg )
cmdify ({ cmds } as model) =
    { model | cmds = [] } ! cmds


updateCurTime : Time -> Model -> Model
updateCurTime timeDelta ({ curTime } as model) =
    { model | curTime = curTime + timeDelta }


spawnQuabs : Model -> Model
spawnQuabs ({ quabs, qQuabs, curTime, effects, cmds } as model) =
    case qQuabs of
        [] ->
            model

        ( timeToSpawn, quab ) :: tail ->
            if timeToSpawn <= curTime then
                spawnQuabs
                    { model
                        | quabs = quab :: quabs
                        , qQuabs = tail
                        , cmds = playWav "crab-hello.wav" :: cmds
                        , effects =
                            { expTime = curTime + splashLongevity
                            , pos = quab.pos
                            , kind = Splash
                            , seed = quab.seed
                            }
                                :: effects
                    }
            else
                model


moveQuabs : Time -> Model -> Model
moveQuabs timeDelta ({ quabs, config } as model) =
    { model | quabs = List.map (moveQuabCloserToNest config timeDelta) quabs }


collideDurdleAndQuabs : Model -> Model
collideDurdleAndQuabs ({ config, durdle, quabs, curTime, kaiju, cmds } as model) =
    let
        ( newQuabs, numCollidedQuabs ) =
            quabs
                |> List.map (collideWithDurdle config curTime durdle)
                |> (\listOfQuabsAndDidCollide ->
                        ( listOfQuabsAndDidCollide
                            |> List.map Tuple.first
                        , listOfQuabsAndDidCollide
                            |> List.filter Tuple.second
                            |> List.length
                        )
                   )

        newDurdle =
            if numCollidedQuabs > 0 then
                bumpDurdle durdle
            else
                durdle
    in
    { model
        | quabs = newQuabs
        , durdle = newDurdle
        , kaiju = kaiju + 2 * toFloat numCollidedQuabs
        , cmds =
            if numCollidedQuabs > 0 then
                playWav "crab-death.wav" :: cmds
            else
                cmds
    }


removeDeadQuabs : Model -> Model
removeDeadQuabs ({ quabs, config, curTime } as model) =
    { model | quabs = List.filter (isAlive config curTime) quabs }


munchTime =
    600


eatEggs : Model -> Model
eatEggs ({ quabs, curTime, numEggs, cmds } as model) =
    quabs
        |> List.map
            (\quab ->
                if doesCollideWithNest quab && (quab.lastAteAt + munchTime < curTime) then
                    ( { quab | lastAteAt = curTime }, True )
                else
                    ( quab, False )
            )
        |> (\quabsAndDidEat ->
                let
                    numEggsEaten =
                        quabsAndDidEat |> List.filter Tuple.second |> List.length
                in
                { model
                    | quabs = List.map Tuple.first quabsAndDidEat
                    , numEggs = numEggs - numEggsEaten
                    , cmds =
                        if numEggsEaten > 0 then
                            playWav "egg-eat.wav" :: cmds
                        else
                            cmds
                }
           )


checkGameOver : Model -> Model
checkGameOver ({ numEggs, cmds } as model) =
    if numEggs > 0 then
        model
    else
        { model
            | state = GameOver
            , cmds = gameOver () :: cmds
        }


checkVictory : Model -> Model
checkVictory ({ curTime, cmds } as model) =
    if curTime < timeUntilHatch then
        model
    else
        { model | state = Victory, cmds = victory () :: cmds }


isAlive : Config -> Time -> Quab -> Bool
isAlive config curTime quab =
    case quab.state of
        Alive ->
            True

        Bouncing _ ->
            -- TODO check offscreen
            True

        Exploding expTime ->
            expTime > curTime


doesCollideWithNest : Quab -> Bool
doesCollideWithNest quab =
    case quab.state of
        Alive ->
            Math.dist nestPos quab.pos < (nestRad + quab.rad)

        _ ->
            False


collideWithDurdle : Config -> Time -> Durdle -> Quab -> ( Quab, Bool )
collideWithDurdle config curTime durdle quab =
    case quab.state of
        Alive ->
            if isTouchingDurdle config durdle quab then
                ( { quab
                    | state =
                        if True then
                            Exploding (curTime + explosionLongevity)
                        else
                            Bouncing
                                (V2.sub quab.pos quab.lastPos
                                    |> V2.negate
                                    |> V2.toTuple
                                    |> toPolar
                                    |> (\( r, a ) ->
                                            ((durdle.angle + turns 0.25) - a)
                                                + (durdle.angle + turns 0.25)
                                       )
                                )
                  }
                , True
                )
            else
                ( quab, False )

        Bouncing _ ->
            ( quab, False )

        Exploding _ ->
            ( quab, False )


baseSpeed =
    0.01


bumpDurdle : Durdle -> Durdle
bumpDurdle durdle =
    let
        ( nestX, nestY ) =
            V2.toTuple nestPos

        ( durdleX, durdleY ) =
            V2.toTuple durdle.pos
    in
    toPolar ( nestX - durdleX, nestY - durdleY )
        |> (\( _, angle ) -> fromPolar ( 3, angle ))
        |> (\( x, y ) ->
                { durdle | pos = V2.fromTuple ( durdleX + x, durdleY + y ) }
           )


moveQuabCloserToNest : Config -> Time -> Quab -> Quab
moveQuabCloserToNest config timeDelta quab =
    case quab.state of
        Alive ->
            if doesCollideWithNest quab then
                quab
            else
                let
                    ( nestX, nestY ) =
                        V2.toTuple nestPos

                    ( quabX, quabY ) =
                        V2.toTuple quab.pos
                in
                toPolar ( nestX - quabX, nestY - quabY )
                    |> (\( _, angle ) -> fromPolar ( timeDelta * config.quabSpeed * baseSpeed, angle ))
                    |> (\( x, y ) ->
                            { quab
                                | pos = V2.fromTuple ( quabX + x, quabY + y )
                                , lastPos = quab.pos
                            }
                       )

        Bouncing angle ->
            { quab
                | pos =
                    fromPolar ( timeDelta * config.quabSpeed * baseSpeed, angle )
                        |> V2.fromTuple
                        |> V2.add quab.pos
                , lastPos = quab.pos
            }

        _ ->
            quab



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions ({ state } as model) =
    Sub.batch
        [ windowChanged WindowChanged
        , case state of
            Playing ->
                AnimationFrame.diffs Tick

            _ ->
                Sub.none
        ]


isTouchingDurdle : Config -> Durdle -> Quab -> Bool
isTouchingDurdle ({ durdleLength, durdleThickness } as config) durdle quab =
    -- check if distance between
    let
        ( a, b, c, d ) =
            getDurdleSweepQuadPoints config durdle

        ( e, f ) =
            ( quab.pos, quab.lastPos )

        minDist =
            List.minimum
                [ Math.getDistBetweenLines ( a, b ) ( e, f )
                , Math.getDistBetweenLines ( b, c ) ( e, f )
                , Math.getDistBetweenLines ( c, d ) ( e, f )
                , Math.getDistBetweenLines ( d, a ) ( e, f )
                ]
                |> Maybe.withDefault -42
    in
    (minDist <= (durdleThickness * durdle.thickness / 2) + quab.rad)
        || (List.length (List.filter (Math.doLinesIntersect ( quab.pos, V2.fromTuple ( -1000, -1000 ) )) [ ( a, b ), ( b, c ), ( c, d ), ( d, a ) ]) % 2 == 1)
