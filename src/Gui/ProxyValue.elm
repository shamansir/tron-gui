module Gui.ProxyValue exposing (..)


import Color


import Json.Decode as D
import Json.Encode as E


import Color exposing (Color)
import Gui.Control.Nest exposing (Id)
import Gui.Control.Toggle exposing (ToggleState, toggleToBool)


type ProxyValue
    = FromSlider Float
    | FromXY ( Float, Float )
    | FromInput String
    | FromChoice Id
    | FromColor Color
    | FromToggle ToggleState
    | FromButton
    | Other



encode : ProxyValue -> E.Value
encode v =
    case v of
        FromSlider f -> E.float f
        FromXY ( x, y ) ->
            E.object
                [ ( "x", E.float x )
                , ( "y", E.float y )
                ]
        FromInput t -> E.string t
        FromChoice i -> E.int i
        FromColor c ->
            case Color.toRgba c of
                { red, green, blue, alpha } ->
                    E.object
                        [ ( "red", E.float red )
                        , ( "green", E.float green )
                        , ( "blue", E.float blue )
                        , ( "alpha", E.float alpha )
                        , ( "css", E.string <| Color.toCssString c )
                        ]
        FromToggle t -> E.bool <| toggleToBool t
        FromButton -> E.string ""
        Other -> E.null
