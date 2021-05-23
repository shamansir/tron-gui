module Tron.Expose.ProxyValue exposing
    ( ProxyValue(..), get
    , encode
    , toString, getTypeString
    , toggleToBool, toggleToString
    , fromNumber, fromXY, fromText, fromChoice, fromChoiceOf, fromColor, fromToggle, fromAction
    )


{-| The helper type to represent any value flowing through the GUI.

Used for converting values from controls to JSON;

@docs ProxyValue

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

import Tron.Control as Control
import Tron.Control.Nest as Nest exposing (ItemId)
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


{-| -}
fromNumber : ProxyValue -> Maybe Float
fromNumber proxy =
    case proxy of
        FromSlider n -> Just n
        _ -> Nothing


{-| -}
fromXY : ProxyValue -> Maybe ( Float, Float )
fromXY proxy =
    case proxy of
        FromXY xy -> Just xy
        _ -> Nothing


{-| -}
fromText : ProxyValue -> Maybe String
fromText proxy =
    case proxy of
        FromInput text -> Just text
        _ -> Nothing


{-| -}
fromChoice : ProxyValue -> Maybe ItemId
fromChoice proxy =
    case proxy of
        FromChoice i -> Just i
        _ -> Nothing


{-| -}
fromChoiceOf : List a -> ProxyValue -> Maybe a
fromChoiceOf values =
    let
        itemsArray = Array.fromList values
    in
        fromChoice
            >> Maybe.andThen
                (\id -> Array.get id itemsArray)


{-| -}
fromToggle : ProxyValue -> Maybe ToggleState
fromToggle proxy =
    case proxy of
        FromToggle state -> Just state
        _ -> Nothing


{-| -}
fromAction : ProxyValue -> Maybe ()
fromAction proxy =
    case proxy of
        FromButton -> Just ()
        _ -> Nothing


{-| -}
fromColor : ProxyValue -> Maybe Color
fromColor proxy =
    case proxy of
        FromColor color -> Just color
        _ -> Nothing


get : Property a -> ProxyValue
get prop =
    case prop of
        Nil -> Other
        Number control -> control |> Control.getValue |> FromSlider
        Coordinate control -> control |> Control.getValue |> FromXY
        Text control -> control |> Control.getValue |> Tuple.second |> FromInput
        Color control -> control |> Control.getValue |> FromColor
        Toggle control -> control |> Control.getValue |> FromToggle
        Action _ -> FromButton
        Choice _ _ control -> control |> Control.getValue |> .selected |> FromChoice
        Group _ _ _ -> Other
