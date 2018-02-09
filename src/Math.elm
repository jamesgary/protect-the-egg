module Math exposing (..)


type alias Pos =
    { x : Float
    , y : Float
    }


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
    42
