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
    { state : HeroState
    , pos : Pos
    , lastPos : Pos
    , angle : Float
    , lastAngle : Float
    , length : Float -- length represents the inner rect, not the circle bumpers
    , thickness : Float
    }


type HeroState
    = Shield
    | Sword


type alias Enemy =
    { pos : Pos
    , lastPos : Pos
    , rad : Float
    }


type Shape
    = Circle { pos : Pos, rad : Float }
    | Rect { pos : Pos, width : Float, height : Float, angle : Float }


type Msg
    = MouseMove Mouse.Position
    | MouseClick Mouse.Position
    | Tick Time



--heroShapes : Hero -> { cur : List Shape, last : List Shape }
--heroShapes ({ pos, lastPos, width, height, angle, lastAngle } as hero) =
--    let
--        -- consider hero to be wider than taller
--        ( width, height ) =
--            if hero.width > hero.height then
--                ( hero.width, hero.height )
--            else
--                ( hero.height, hero.width )
--
--        ( rotOffsetX, rotOffsetY ) =
--            fromPolar ( width / 2, angle )
--
--        ( rotOffsetXLast, rotOffsetYLast ) =
--            fromPolar ( width / 2, lastAngle )
--    in
--    { cur =
--        [ Circle { pos = { x = pos.x + rotOffsetX, y = pos.y + rotOffsetY }, rad = height / 2 }
--        , Circle { pos = { x = pos.x - rotOffsetX, y = pos.y - rotOffsetY }, rad = height / 2 }
--        , Rect { pos = hero.pos, width = width, height = height, angle = angle }
--        ]
--    , last =
--        [ Circle { pos = { x = lastPos.x + rotOffsetXLast, y = lastPos.y + rotOffsetYLast }, rad = height / 2 }
--        , Circle { pos = { x = lastPos.x - rotOffsetXLast, y = lastPos.y - rotOffsetYLast }, rad = height / 2 }
--        , Rect { pos = hero.lastPos, width = width, height = height, angle = lastAngle }
--        , let
--            x =
--                (pos.x + lastPos.x) / 2
--
--            y =
--                (pos.y + lastPos.y) / 2
--
--            a =
--                (angle + lastAngle) / 2
--          in
--          Rect { pos = { x = x, y = y }, width = width + height, height = dist pos lastPos, angle = a }
--        ]
--    }


dist : Pos -> Pos -> Float
dist pos1 pos2 =
    sqrt ((pos1.x - pos2.x) ^ 2 + (pos1.y - pos2.y) ^ 2)



-- Game 2d stuff


camera =
    Camera.fixedHeight (16 * 9) ( 0, 0 )


cameraWidth =
    800


cameraHeight =
    450
