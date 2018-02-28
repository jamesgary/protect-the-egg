port module Ports exposing (..)

import Common


port windowChanged : (( Int, Int ) -> msg) -> Sub msg


port playWav : String -> Cmd msg


port playSong : () -> Cmd msg


port pauseSong : () -> Cmd msg


port stopSong : () -> Cmd msg
