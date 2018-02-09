module Math exposing (..)

import Math.Vector2 as V2 exposing (Vec2)


type alias Pos =
    Vec2


getDistBetweenLines : ( Pos, Pos ) -> ( Pos, Pos ) -> Float
getDistBetweenLines ( a, b ) ( c, d ) =
    List.minimum
        [ getDistBetweenLineAndPoint ( a, b ) c
        , getDistBetweenLineAndPoint ( a, b ) d
        , getDistBetweenLineAndPoint ( c, d ) a
        , getDistBetweenLineAndPoint ( c, d ) b
        ]
        |> Maybe.withDefault -42


getDistBetweenLineAndPoint : ( Pos, Pos ) -> Pos -> Float
getDistBetweenLineAndPoint ( a, b ) c =
    -- https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Line_defined_by_two_points
    let
        ( x1, y1 ) =
            V2.toTuple a

        ( x2, y2 ) =
            V2.toTuple b

        ( x0, y0 ) =
            V2.toTuple c
    in
    abs (((y2 - y1) * x0) - ((x2 - x1) * y0) + (x2 * y1) - (y2 * x1))
        / sqrt (((y2 - y1) ^ 2) + ((x2 - x1) ^ 2))


dist : Vec2 -> Vec2 -> Float
dist pos1 pos2 =
    let
        ( x1, y1 ) =
            V2.toTuple pos1

        ( x2, y2 ) =
            V2.toTuple pos2
    in
    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
