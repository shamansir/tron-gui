module Gui.Msg exposing (..)


import HashId exposing (HashId)

import Gui.Path exposing (Path)
import Gui.Mouse exposing (MouseAction)
import Gui.Expose as Exp


type Msg_
    = NoOp
    | ApplyMouse MouseAction -- from the document
    | ViewportChanged ( Int, Int )
    | Click Path
    | MouseDown Path
    | KeyDown Int
    | TextInput Path String
    | Detach Path
    | ReceiveRaw Exp.RawUpdate
    | SetClientId HashId
