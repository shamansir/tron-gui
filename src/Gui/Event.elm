module Gui.Event exposing (..)


import Gui.Path exposing (Path)
import Gui.Property exposing (Property)


type Event
    = Click
    | DragStart ( Float, Float )
    | Drag ( Float, Float )
    | DragEnd ( Float, Float )
    | Key Int
    | AtChild Path Event
    | Batch (List Event)


type alias Updates msg = ( List ( Path, Property msg ), Cmd msg )


-- TODO: move `Property.execute`, `Property.executeAt`, `Gui.handleKeyDown`, `Gui.handleMouse` here


consumeKey : Int -> Property msg -> Bool
consumeKey keyCode prop = False


dispatch : Event -> Property msg -> Path -> Property msg -> Updates msg
dispatch event parent path prop = ( [], Cmd.none )
