port module Ports exposing (..)

import Common


port windowChanged : (( Int, Int ) -> msg) -> Sub msg


port playWav : String -> Cmd msg
