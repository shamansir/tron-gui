module Tron.Control.Impl.Text exposing (..)


import Tron.Control as Core exposing (Control)
import Tron.Control as Control exposing (..)
import Tron.Control.Action as A


type TextState
    = Ready
    | Editing


type alias Control a = Core.Control () ( TextState, String ) a


type alias Transient =
    TextState


update : A.Action -> Control a -> ( Control a, A.Change )
update action control =
    case action of
        A.Execute ->
            ( ensureEditing control, A.Fire )
        A.Exit ->
            ( finishEditing control, A.Fire )
        _ ->
            ( control, A.Stay )


ensureEditing : Control a -> Control a
ensureEditing =
    Control.update <| \(_, v) -> ( Editing, v )


finishEditing : Control a -> Control a
finishEditing =
    Control.update <| \(_, v) -> ( Ready, v )


updateText : String -> Control a -> Control a
updateText newValue =
    Core.update
        <| \(s, _) -> ( s, newValue )


getTransientState : Control a -> Transient
getTransientState (Core.Control _ ( state, _ ) _) = state


restoreTransientState : Control a -> Transient -> Control a
restoreTransientState (Control setup ( _, val ) handler) state =
    Core.Control setup ( state, val ) handler
