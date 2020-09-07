module Simple.Msg exposing (..)


import Gui.Property as Gui exposing (Color)
import Simple.Model exposing (..)


type Msg
    = NoOp
    | ChangeInt Int
    | ChangeFloat Float
    | ChangeXY ( Float, Float )
    | ChangeString String
    | ChangeColor Gui.Color
    | Choose Choice
    | Switch Bool
    | Pressed Choice
