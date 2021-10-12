module Tron.Control.Impl.Toggle exposing (..)


import Tron.Control as Core exposing (Control)
import Tron.Control as Control exposing (..)


type ToggleState
    = TurnedOn
    | TurnedOff


type alias Control a = Core.Control () ToggleState a


toggle : Control a -> Control a
toggle =
    Control.update
        <| \current ->
            case current of
                TurnedOff -> TurnedOn
                TurnedOn -> TurnedOff


toggleOn : Control a -> Control a
toggleOn =
    Control.update <| always TurnedOn


toggleOff : Control a -> Control a
toggleOff =
    Control.update <| always TurnedOff


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


toggleToString : ToggleState -> String
toggleToString state =
    case state of
        TurnedOn -> "on"
        TurnedOff -> "off"


stringToToggle : String -> Maybe ToggleState
stringToToggle string =
    case string of
        "on" -> Just TurnedOn
        "off" -> Just TurnedOff
        _ -> Nothing
