module Tron.Control.Impl.Text exposing (..)


import Tron.Control as Core exposing (Control)
import Tron.Control as Control exposing (..)


type TextState
    = Ready
    | Editing


type alias Control a = Core.Control () ( TextState, String ) a


type alias Transient =
    TextState


ensureEditing : Control a -> Control a
ensureEditing =
    Control.update <| \(_, v) -> ( Editing, v )


finishEditing : Control a -> Control a
finishEditing =
    Control.update <| \(_, v) -> ( Ready, v )


updateText : String -> Control a -> Control a
updateText newValue =
    update
        <| \(s, _) -> ( s, newValue )


getTransientState : Control a -> Transient
getTransientState (Core.Control _ ( state, _ ) _) = state


restoreTransientState : Control a -> Transient -> Control a
restoreTransientState (Control setup ( _, val ) handler) state =
    Core.Control setup ( state, val ) handler
