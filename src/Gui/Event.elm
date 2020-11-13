module Gui.Event exposing (..)


import Gui.Path exposing (Path)
import Gui.Property exposing (Property)


type Event
    = Click
    | DragStart ( Float, Float )
    | Drag ( Float, Float )
    | DragEnd ( Float, Float )
    --| Focus
    | Key Int
    | Batch (List Event)


type alias Updates msg = ( List ( Path, Property msg ), Cmd msg )


-- TODO: move `Property.execute`, `Property.executeAt`, `Gui.handleKeyDown`, `Gui.handleMouse` here


dispatch : Event -> Property msg -> Path -> Property msg -> Updates msg
dispatch event root path prop = ( [], Cmd.none )
