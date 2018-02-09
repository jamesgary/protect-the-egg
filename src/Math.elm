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

        lengthSq =
            V2.distanceSquared a b
    in
    if lengthSq == 0 then
        V2.distance c a
    else
        let
            t =
                max 0 (min 1 (V2.dot (V2.sub c a) (V2.sub b a) / lengthSq))

            projection =
                V2.add a (V2.scale t (V2.sub b a))
        in
        V2.distance c projection


dist : Vec2 -> Vec2 -> Float
dist pos1 pos2 =
    let
        ( x1, y1 ) =
            V2.toTuple pos1

        ( x2, y2 ) =
            V2.toTuple pos2
    in
    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)



-- MAY NOT NEED?


type alias Line =
    ( Vec2, Vec2 )


doesLineIntersectLines : Line -> List Line -> Bool
doesLineIntersectLines line lines =
    List.any (doLinesIntersect line) lines


doLinesIntersect : Line -> Line -> Bool
doLinesIntersect line1 line2 =
    -- https://stackoverflow.com/a/24392281
    let
        ( line1A, line1B ) =
            line1

        ( line2A, line2B ) =
            line2

        ( a, b ) =
            V2.toTuple line1A

        ( c, d ) =
            V2.toTuple line1B

        ( p, q ) =
            V2.toTuple line2A

        ( r, s ) =
            V2.toTuple line2B

        det =
            (c - a) * (s - q) - (r - p) * (d - b)
    in
    if det == 0 then
        False
    else
        let
            lambda =
                ((s - q) * (r - a) + (p - r) * (s - b)) / det

            gamma =
                ((b - d) * (r - a) + (c - a) * (s - b)) / det
        in
        (0 < lambda && lambda < 1) && (0 < gamma && gamma < 1)
