module Common exposing (..)

import ElementRelativeMouseEvents as Mouse
import Game.Resources as Resources exposing (Resources)
import Game.TwoD.Camera as Camera
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
import Random
import Time exposing (Time)


type alias Model =
    { viewportWidth : Int
    , viewportHeight : Int
    , canvasSize : Int
    , sidebarWidth : Int
    , hero : Hero
    , enemies : List Enemy
    , seed : Random.Seed
    , timeSinceLastSpawn : Time
    , curTime : Time
    , config : Config
    , mousePos : Vec2
    , qEnemies : List ( Time, Enemy ) -- queued enemies
    , resources : Resources
    , cmds : List (Cmd Msg)
    , kaiju : Float
    , numEggs : Int
    , state : GameState
    , isStartBtnHovered : Bool
    , effects : List Effect
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
    { heroLength : Float
    , heroThickness : Float
    , enemySpeed : Float
    , enemySpawnRate : Float
    , enemyClusterSize : Int
    }


type alias Flag =
    { viewportWidth : Int
    , viewportHeight : Int
    , timestamp : Int
    }


type alias Hero =
    { state : HeroState
    , pos : Vec2
    , lastPos : Vec2
    , vel : Vec2
    , angle : Float
    , lastAngle : Float
    , length : Float -- length represents the inner rect, not the circle bumpers
    , thickness : Float
    }


type HeroState
    = Shield
    | Sword


type alias Enemy =
    { pos : Vec2
    , lastPos : Vec2
    , rad : Float
    , state : EnemyState
    , seed : Random.Seed
    , lastAteAt : Time
    }


type EnemyState
    = Alive
    | Bouncing Float
    | Exploding Time


type Shape
    = Circle { pos : Vec2, rad : Float }
    | Rect { pos : Vec2, width : Float, height : Float, angle : Float }


explosionLongevity =
    1000


type Msg
    = MouseMove Mouse.Point
    | MouseClick Mouse.Point
    | Tick Time
    | TogglePause
    | ChangeHeroLength String
    | ChangeHeroThickness String
    | ChangeEnemySpawnRate String
    | ChangeEnemySpeed String
    | ChangeEnemyClusterSize String
    | WindowChanged ( Int, Int )
    | Resources Resources.Msg
    | MouseOnStartBtn
    | MouseOutStartBtn
    | StartGame
    | TryAgain


getHeroSweepQuadPoints : Config -> Hero -> ( Vec2, Vec2, Vec2, Vec2 )
getHeroSweepQuadPoints { heroLength } { pos, lastPos, length, angle, lastAngle } =
    let
        ( rotOffsetX, rotOffsetY ) =
            fromPolar ( heroLength * length / 2, angle )

        ( rotOffsetXLast, rotOffsetYLast ) =
            fromPolar ( heroLength * length / 2, lastAngle )

        ( x, y ) =
            V2.toTuple pos

        ( lastX, lastY ) =
            V2.toTuple lastPos

        -- assuming width is longer than height...
        a =
            V2.fromTuple ( x - rotOffsetX, y - rotOffsetY )

        b =
            V2.fromTuple ( x + rotOffsetX, y + rotOffsetY )

        c =
            V2.fromTuple ( lastX + rotOffsetXLast, lastY + rotOffsetYLast )

        d =
            V2.fromTuple ( lastX - rotOffsetXLast, lastY - rotOffsetYLast )
    in
    ( a, b, c, d )



-- config applications


trueLength : Config -> Hero -> Float
trueLength { heroLength } { length } =
    heroLength * length


trueThickness : Config -> Hero -> Float
trueThickness { heroThickness } { thickness } =
    heroThickness * thickness


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


enemyStartingDistFromNest =
    150
