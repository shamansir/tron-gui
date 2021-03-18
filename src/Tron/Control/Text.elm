module Tron.Control.Text exposing (..)


import Tron.Control as Core exposing (Control)
import Tron.Control as Control exposing (..)


type TextState
    = Ready
    | Editing


type alias Control msg = Core.Control () ( TextState, String ) msg


ensureEditing : Control msg -> Control msg
ensureEditing =
    Control.update <| \(_, v) -> ( Editing, v )


finishEditing : Control msg -> Control msg
finishEditing =
    Control.update <| \(_, v) -> ( Ready, v )


updateText : String -> Control msg -> Control msg
updateText newValue =
    update
        <| \(s, _) -> ( s, newValue )
