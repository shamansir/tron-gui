module Simple.Msg exposing (..)


import Color exposing (Color)
import Simple.Model exposing (..)


type Msg
    = NoOp
    | ChangeInt Int
    | ChangeFloat Float
    | ChangeXY ( Float, Float )
    | ChangeString String
    | ChangeColor Color
    | Choose Choice
    | Switch Bool
    | Pressed Choice
