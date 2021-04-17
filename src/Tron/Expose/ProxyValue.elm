module Tron.Expose.ProxyValue exposing
    ( ProxyValue(..)
    , encode
    , toString, getTypeString
    , toggleToBool, toggleToString
    )


{-| The helper type to represent any value flowing through the GUI.

Used for converting values from controls to JSON;

@docs ProxyValue

# Encode and convert

@docs encode, toString, getTypeString

# Extract Toggle value

@docs toggleToBool, toggleToString
-}

import Color
import Color exposing (Color)
import Color.Convert as Color

import Json.Decode as D
import Json.Encode as E

import Tron.Control.Nest exposing (ItemId)
import Tron.Control.Toggle as Toggle exposing (ToggleState, toggleToBool, toggleToString)
import Tron.Control.XY as XY
import Tron.Property exposing (Property(..))


{-| -}
type ProxyValue
    = FromSlider Float
    | FromXY ( Float, Float )
    | FromInput String
    | FromChoice ItemId
    | FromColor Color
    | FromToggle ToggleState
    | FromButton
    | Other



{-| Encode value to JSON. -}
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


{-| Encode value to string (regardless of the type). -}
toString : ProxyValue -> String
toString v =
    case v of
        FromSlider f ->
            String.fromFloat f
        FromXY ( x, y ) ->
            String.fromFloat x ++ XY.separator ++ String.fromFloat y
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


{-| Get type of the value as string. -}
getTypeString :
    ProxyValue
    -> String
getTypeString value =
    case value of
        Other ->
            "ghost"

        FromSlider _ ->
            "slider"

        FromXY _ ->
            "xy"

        FromInput _ ->
            "text"

        FromColor _ ->
            "color"

        FromChoice _ ->
            "choice"

        FromToggle _ ->
            "toggle"

        FromButton ->
            "button"


{-| -}
toggleToBool : ToggleState -> Bool
toggleToBool = Toggle.toggleToBool


{-| -}
toggleToString : ToggleState -> String
toggleToString = Toggle.toggleToString
