module Gui.Control.Toggle exposing (..)


import Gui.Control as Core exposing (Control)
import Gui.Control as Control exposing (..)


type ToggleState
    = TurnedOn
    | TurnedOff


type alias Control msg = Core.Control () ToggleState msg


doToggle : Control msg -> Control msg
doToggle =
    Control.update
        <| \current ->
            case current of
                TurnedOff -> TurnedOn
                TurnedOn -> TurnedOff


toggleToBool : ToggleState -> Bool
toggleToBool state =
    case state of
        TurnedOn -> True
        TurnedOff -> False


boolToToggle : Bool -> ToggleState
boolToToggle bool =
    case bool of
        True -> TurnedOn
        False -> TurnedOff


