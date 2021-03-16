module Gui.ProxyValue exposing (..)


import Color
import Color.Convert as Color


import Json.Decode as D
import Json.Encode as E


import Color exposing (Color)
import Gui.Control.Nest exposing (ItemId)
import Gui.Control.Toggle exposing (ToggleState, toggleToBool, toggleToString)


type ProxyValue
    = FromSlider Float
    | FromXY ( Float, Float )
    | FromInput String
    | FromChoice ItemId
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
                        , ( "css", E.string <| Color.colorToHexWithAlpha c )
                        ]
        FromToggle t -> E.bool <| toggleToBool t
        FromButton -> E.string ""
        Other -> E.null


toString : ProxyValue -> String
toString v =
    case v of
        FromSlider f ->
            String.fromFloat f
        FromXY ( x, y ) ->
            String.fromFloat x ++ ":" ++ String.fromFloat y
        FromInput t ->
            t
        FromChoice i ->
            String.fromInt i
        FromColor c ->
            Color.colorToHexWithAlpha c
        FromToggle t ->
            toggleToString t
        FromButton ->
            ""
        Other ->
            ""
