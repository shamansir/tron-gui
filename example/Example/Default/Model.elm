module Example.Default.Model exposing (..)


import Color exposing (Color)


type Choice = A | B | C | D


choices : List Choice
choices = [ A, B, C, D ]


type alias Model =
    { int : Int
    , float : Float
    , string : String
    , choice : Choice
    , color : Color
    , toggle : Bool
    , xy : ( Float, Float )
    , buttonPressed : Choice
    }


init : Model
init =
    { int = 0
    , float = 0.0
    , string = "foobar"
    , choice = A
    , color = Color.rgb255 255 194 0
    , toggle = False
    , xy = ( 0, 0 )
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
