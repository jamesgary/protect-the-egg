module Common exposing (..)

import Game.TwoD.Camera as Camera exposing (Camera)
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
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
    , timestamp : Int
    }


type alias Egg =
    { pos : Pos
    , rad : Float
    }


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Hero =
    { pos : Pos
    , angle : Float
    , width : Float
    , height : Float
    }


type alias Enemy =
    { pos : Pos
    , rad : Float
    }


type Msg
    = MouseMove Mouse.Position
    | Tick Time



-- Game 2d stuff


camera =
    Camera.fixedHeight (16 * 9) ( 0, 0 )


cameraWidth =
    800


cameraHeight =
    450
