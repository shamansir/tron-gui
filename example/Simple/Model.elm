module Simple.Model exposing (..)


import Gui.GuiAlt as Gui exposing (Color)


type Choice = A | B | C | D


choices : List Choice
choices = [ A, B, C, D ]


type alias Model =
    { int : Int
    , float : Float
    , string : String
    , choice : Choice
    , color : Gui.Color
    , toggle : Bool
    , buttonPressed : Choice
    }


init : Model
init =
    { int = 0
    , float = 0.0
    , string = ""
    , choice = A
    , color = "#ffc200"
    , toggle = False
    , buttonPressed = C
    }


compareChoices : Choice -> Choice -> Bool
compareChoices cA cB =
    case ( cA, cB ) of
        ( A, A ) -> True
        ( B, B ) -> True
        ( C, C ) -> True
        ( D, D ) -> True
        _ -> False


choiceToString : Choice -> String
choiceToString c =
    case c of
        A -> "A"
        B -> "B"
        C -> "C"
        D -> "D"
