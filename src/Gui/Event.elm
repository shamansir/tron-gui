module Gui.Event exposing (..)


type Event
    = Click
    | DragStart ( Float, Float )
    | Drag ( Float, Float )
    | DragEnd ( Float, Float )
    | Focus
    | LoseFocus
    | Key Int
    | Batch (List Event)
