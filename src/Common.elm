module Common exposing (..)

import ElementRelativeMouseEvents as Mouse
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera as Camera
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
import Random
import Time exposing (Time)


type alias Model =
    { durdle : Durdle
    , quabs : List Quab
    , qQuabs : List ( Time, Quab ) -- queued quabs
    , numEggs : Int
    , kaiju : Float
    , state : GameState

    -- game meta logic
    , curTime : Time
    , mousePos : Vec2
    , seed : Random.Seed
    , config : Config

    -- view logic
    , viewportWidth : Int
    , viewportHeight : Int
    , canvasSize : Int
    , sidebarWidth : Int
    , resources : Resources
    , isStartBtnHovered : Bool
    , effects : List Effect

    -- other
    , cmds : List (Cmd Msg)
    }


type GameState
    = Start
    | Paused
    | Playing
    | GameOver
    | Victory


type alias Effect =
    { expTime : Time
    , pos : Vec2
    , seed : Random.Seed
    , kind : EffectKind
    }


type EffectKind
    = Splash


type alias Config =
    { durdleLength : Float
    , durdleThickness : Float
    , quabSpeed : Float
    , quabSpawnRate : Float
    , quabClusterSize : Int
    }


type alias Flag =
    { viewportWidth : Int
    , viewportHeight : Int
    , timestamp : Int
    }


type alias Durdle =
    { state : DurdleState
    , pos : Vec2
    , lastPos : Vec2
    , vel : Vec2
    , angle : Float
    , lastAngle : Float
    , length : Float -- length represents the inner rect, not the circle bumpers
    , thickness : Float
    }


type DurdleState
    = Shield
    | Sword -- aka KAIJU


type alias Quab =
    { pos : Vec2
    , lastPos : Vec2
    , rad : Float
    , state : QuabState
    , seed : Random.Seed
    , lastAteAt : Time
    }


type QuabState
    = Alive
    | Bouncing Float
    | Exploding Time


type Shape
    = Circle { pos : Vec2, rad : Float }
    | Rect { pos : Vec2, width : Float, height : Float, angle : Float }


explosionLongevity =
    1000


type Msg
    = -- user actions
      MouseMove Mouse.Point
    | MouseClick Mouse.Point
    | TogglePause
    | WindowChanged ( Int, Int )
    | StartGame
    | TryAgain
      -- start screen
    | MouseOnStartBtn
    | MouseOutStartBtn
      -- non-user actions
    | Tick Time
    | Resources Resources.Msg
      -- config stuff
    | ChangeDurdleLength String
    | ChangeDurdleThickness String
    | ChangeQuabSpawnRate String
    | ChangeQuabSpeed String
    | ChangeQuabClusterSize String



-- config applications


trueLength : Config -> Durdle -> Float
trueLength { durdleLength } { length } =
    durdleLength * length


trueThickness : Config -> Durdle -> Float
trueThickness { durdleThickness } { thickness } =
    durdleThickness * thickness


nestRad =
    10


beachRad =
    87


quabSpawnDist =
    beachRad + 5



-- Game 2d stuff
-- 200x200, with 0,0 as the center (top left is -100,-100, bottom right is 100,100)


camera =
    Camera.fixedArea (200 * 200) ( 0, 0 )


getDims : Int -> Int -> ( Int, Int )
getDims vWidth vHeight =
    -- sidebar will always be 50% as wide as the canvas
    -- what is the biggest 3:2 rectangle that can fit?
    let
        ratio =
            3 / 2

        ( w, h ) =
            ( vWidth |> toFloat
            , vHeight |> toFloat
            )

        -- square canvas with half-width sidebar
        canvasSize =
            if w / h > ratio then
                -- too wide!
                h
            else
                -- too tall!
                w / ratio
    in
    ( canvasSize |> round
    , canvasSize * 0.5 |> round
    )


splashLongevity =
    1000


timeUntilHatch =
    -- Length of pharaos song
    93 * 1000


nestPos =
    V2.fromTuple ( 0, 0 )


quabStartingDistFromNest =
    150
