module Default.Msg exposing (..)


import Color exposing (Color)
import Default.Model exposing (..)


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
