module Simple.Msg exposing (..)


import Gui.GuiAlt as Gui exposing (Color)
import Simple.Model exposing (..)


type Msg
    = ChangeInt Int
    | ChangeFloat Float
    | ChangeString String
    | ChangeColor Gui.Color
    | Choose Choice
    | Switch Bool
    | Pressed Choice
