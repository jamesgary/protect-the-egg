module Common exposing (..)

import Game.TwoD.Camera as Camera exposing (Camera)
import Math.Vector2 as V2 exposing (Vec2)
import Mouse
import Random
import Time exposing (Time)


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , egg : Egg
    , hero : Hero
    , enemies : List Enemy
    , isGameOver : Bool
    , seed : Random.Seed
    , timeSinceLastSpawn : Time
    , curTime : Time
    , config : Config
    }


type alias Config =
    { isPaused : Bool
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
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
    }


type Shape
    = Circle { pos : Vec2, rad : Float }
    | Rect { pos : Vec2, width : Float, height : Float, angle : Float }


type Msg
    = MouseMove Mouse.Position
    | MouseClick Mouse.Position
    | Tick Time
    | TogglePause


getHeroSweepQuadPoints : Hero -> ( Vec2, Vec2, Vec2, Vec2 )
getHeroSweepQuadPoints { pos, lastPos, length, angle, lastAngle } =
    let
        ( rotOffsetX, rotOffsetY ) =
            fromPolar ( length / 2, angle )

        ( rotOffsetXLast, rotOffsetYLast ) =
            fromPolar ( length / 2, lastAngle )

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



-- Game 2d stuff


camera =
    Camera.fixedHeight (16 * 9) ( 0, 0 )


cameraWidth =
    800


cameraHeight =
    450
