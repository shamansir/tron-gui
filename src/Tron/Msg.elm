module Tron.Msg exposing (..)


import HashId exposing (HashId)

import Tron.Path exposing (Path)
import Tron.Mouse exposing (MouseAction)
import Tron.Tree.Expose.Tree as Exp
import Tron.Pages as Pages


type Msg_
    = NoOp
    | ApplyMouse MouseAction -- from the document
    | ViewportChanged ( Int, Int )
    | Click Path
    | MouseDown Path
    | KeyDown Int
    | TextInput Path String
    | SwitchPage Path Pages.PageNum
    -- | NextPage Path
    -- | PrevPage Path
    | Detach Path
