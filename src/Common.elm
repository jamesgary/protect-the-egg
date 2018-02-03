module Common exposing (..)

import Mouse
import Time exposing (Time)


type alias Model =
    { windowWidth : Int
    , windowHeight : Int
    , egg : Egg
    , hero : Hero
    , enemies : List Enemy
    , isGameOver : Bool
    }


type alias Flag =
    { windowWidth : Int
    , windowHeight : Int
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
    , rad : Float
    , angle : Float
    }


type alias Enemy =
    { pos : Pos
    , rad : Float
    }


type Msg
    = MouseMove Mouse.Position
    | Tick Time
