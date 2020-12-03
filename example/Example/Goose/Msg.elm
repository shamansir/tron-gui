module Example.Goose.Msg exposing (..)


import Color exposing (Color)
import Example.Goose.Model exposing (LookDirection, Shoes)


type Msg
    = HonkOn
    | HonkOff
    | PunkOn
    | PunkOff
    | ChangeHonkText String
    | ChangeHonkTextSize Int
    | ChangeHonkTextPosition ( Float, Float )
    | ChangeHonkTextColor Color
    | ChangeEyePosition ( Float, Float )
    | ChangeEyeSize Float
    | LookAt LookDirection
    | ChangeShoes Shoes
    | ChangeFeathersColor Color
    | ChangeSkinColor Color
    | ChangeEyeColor Color
    | ChangeIroquoisColor Color
    | ChangeBackground Color
