module Common exposing (..)

import ElementRelativeMouseEvents as Mouse
import Game.TwoD.Camera as Camera exposing (Camera)
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
import Random
import Time exposing (Time)


type alias Model =
    { cameraWidth : Int
    , cameraHeight : Int
    , egg : Egg
    , hero : Hero
    , enemies : List Enemy
    , isGameOver : Bool
    , seed : Random.Seed
    , timeSinceLastSpawn : Time
    , curTime : Time
    , config : Config
    , mousePos : Vec2
    }


type alias Config =
    { isPaused : Bool
    , heroLength : Float
    , heroThickness : Float
    , enemySpeed : Float
    , enemySpawnRate : Float
    , enemyClusterSize : Int
    }


type alias Flag =
    { cameraWidth : Int
    , cameraHeight : Int
    , timestamp : Int
    }


type alias Egg =
    { pos : Vec2
    , rad : Float
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


getHeroSweepQuadPoints : Config -> Hero -> ( Vec2, Vec2, Vec2, Vec2 )
getHeroSweepQuadPoints { heroLength, heroThickness } { pos, lastPos, length, angle, lastAngle } =
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



-- Game 2d stuff


camera =
    --Camera.fixedHeight (16 * 9) ( 0, 0 )
    Camera.fixedHeight (12 * 12) ( 0, 0 )
