module Tron.Msg exposing (..)


import HashId exposing (HashId)

import Tron.Path exposing (Path)
import Tron.Mouse exposing (MouseAction)
import Tron.Expose as Exp


type Msg_
    = NoOp
    | ApplyMouse MouseAction -- from the document
    | ViewportChanged ( Int, Int )
    | Click Path
    | MouseDown Path
    | KeyDown Int
    | TextInput Path String
    | Detach Path
