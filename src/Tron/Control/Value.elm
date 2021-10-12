module Tron.Control.Value exposing
    ( Value(..)
    , encode
    , toString, getTypeString
    , toggleToBool, toggleToString
    , fromNumber, fromXY, fromText, fromChoice, fromChoiceOf, fromColor, fromToggle, fromAction
    )


{-| The helper type to represent any value flowing through the GUI.

Used for converting values from controls to JSON;

@docs Value, get, lift

# Encode and convert

@docs encode, toString, getTypeString

# Extract value

@docs fromNumber, fromXY, fromText, fromChoice, fromChoiceOf, fromColor, fromToggle, fromAction

# Extract Toggle value

@docs toggleToBool, toggleToString
-}

import Array
import Color
import Color exposing (Color)
import Color.Convert as Color

import Json.Decode as D
import Json.Encode as E

--import Tron exposing (Tron)
import Tron.Control as Control
import Tron.Control.Impl.Nest as Nest exposing (ItemId)
import Tron.Control.Impl.Toggle as Toggle exposing (ToggleState, toggleToBool, toggleToString)
import Tron.Control.Impl.XY as XY


{-| -}
type Value
    = FromSlider Float
    | FromXY ( Float, Float )
    | FromInput String
    | FromChoice ItemId
    | FromColor Color
    | FromToggle ToggleState
    | FromButton
    | FromSwitch Int -- ( Int, String )
    | FromGroup
    | None



{-| Encode value to JSON. -}
encode : Value -> E.Value
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
        FromGroup -> E.string ""
        FromSwitch i -> E.int i
        None -> E.null


{-| Encode value to string (regardless of the type). -}
toString : Value -> String
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
        FromGroup ->
            ""
        FromSwitch i ->
            String.fromInt i
        None ->
            ""


{-| Get type of the value as string. -}
getTypeString :
    Value
    -> String
getTypeString value =
    case value of
        None ->
            "none"

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

        FromSwitch _ ->
            "switch"

        FromGroup ->
            "nest"


{-| -}
toggleToBool : ToggleState -> Bool
toggleToBool = Toggle.toggleToBool


{-| -}
toggleToString : ToggleState -> String
toggleToString = Toggle.toggleToString


{-| -}
fromNumber : Value -> Maybe Float
fromNumber proxy =
    case proxy of
        FromSlider n -> Just n
        _ -> Nothing


{-| -}
fromXY : Value -> Maybe ( Float, Float )
fromXY proxy =
    case proxy of
        FromXY xy -> Just xy
        _ -> Nothing


{-| -}
fromText : Value -> Maybe String
fromText proxy =
    case proxy of
        FromInput text -> Just text
        _ -> Nothing


{-| -}
fromChoice : Value -> Maybe ItemId
fromChoice proxy =
    case proxy of
        FromChoice i -> Just i
        _ -> Nothing


{-| -}
fromChoiceOf : List a -> Value -> Maybe a
fromChoiceOf values =
    let
        itemsArray = Array.fromList values
    in
        fromChoice
            >> Maybe.andThen
                (\id -> Array.get id itemsArray)


{-| -}
fromToggle : Value -> Maybe ToggleState
fromToggle proxy =
    case proxy of
        FromToggle state -> Just state
        _ -> Nothing


{-| -}
fromAction : Value -> Maybe ()
fromAction proxy =
    case proxy of
        FromButton -> Just ()
        _ -> Nothing


{-| -}
fromColor : Value -> Maybe Color
fromColor proxy =
    case proxy of
        FromColor color -> Just color
        _ -> Nothing