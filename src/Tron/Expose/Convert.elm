module Tron.Expose.Convert
    exposing
    ( toExposed, toProxied, toStrExposed, toUnit
    , reflect, lift, evaluate
    )


{-| Make your `Tron *` store the additional information along with messages,
or just get rid of messages at all:

@docs toUnit, toProxied, toExposed, toStrExposed
-}


import Json.Encode as E

import Tron exposing (Tron)
import Tron.Control as Control
import Tron.Control.Nest as Nest
import Tron.Expose.Data exposing (..)
import Tron.Path as Path
import Tron.Property as Property exposing (..)
import Tron.Expose.ProxyValue as ProxyValue exposing (ProxyValue(..))


{-| Instead of messages, store nothing, but values.
-}
toUnit : Tron a -> Tron ()
toUnit =
    map <| always ()


{-| Store a `ProxyValue` together with message, which mirrors a value
as a data type to make it easier to connect with other APIs, where message is not
important, such as `dat.gui`, or send it to ports along with sending it to user.

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
toProxied : Tron a -> Tron ( ProxyValue, a )
toProxied prop =
    let
        convertWith f =
            Control.reflect
                >> Control.map (Tuple.mapFirst f)
    in
    case prop of
        Nil ->
            Nil

        Number control ->
            control
                |> convertWith (Tuple.second >> FromSlider)
                |> Number

        Coordinate control ->
            control
                |> convertWith (Tuple.second >> FromXY)
                |> Coordinate

        Text control ->
            control
                |> convertWith (Tuple.second >> FromInput)
                |> Text

        Color control ->
            control
                |> convertWith (Tuple.second >> FromColor)
                |> Color

        Toggle control ->
            control
                |> convertWith FromToggle
                |> Toggle

        Action control ->
            control
                |> convertWith (always FromButton)
                |> Action

        Choice focus shape control ->
            control
                |> Nest.mapItems (Tuple.mapSecond toProxied)
                |> convertWith (.selected >> FromChoice)
                |> Choice focus shape

        Group focus shape control ->
            control
                |> Nest.mapItems (Tuple.mapSecond toProxied)
                |> convertWith (always Other)
                -- TODO: notify expanded/collapsed/detached?
                |> Group focus shape



-- FIXME: make it: Tron msg -> Tron RawOutUpdate and use `Property.map2` to join

{-| Store a `RawOutUpdate` together with message, which is a package that stores all
the required information about the value, such as:

- the path to it in the Tree, both with labels and integer IDs;
- the value in JSON;
- the value as a string;
- the type of the value, as a string;
- client ID, for the communication with WebSockets; (will be removed in future versions)

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
toExposed : Tron a -> Tron ( RawValue, a )
toExposed =
    toProxied
        >> Property.addPaths
        -- FIXME: `Expose.encodeUpdate` does the same as above
        >> Property.map
            (\( ( path, labelPath ), ( proxyVal, msg ) ) ->
                ( { path = Path.toList path
                  , labelPath = labelPath
                  , type_ = ProxyValue.getTypeString proxyVal
                  , value = ProxyValue.encode proxyVal
                  , stringValue = ProxyValue.toString proxyVal
                  }
                , msg
                )
            )



-- FIXME: make it: Tron msg -> Tron ( LabelPath, String ) and use `Tron.map2` to join

{-| Store a labeled path (such as `honk/color`) to the property and its stringified value,
together with message.

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
toStrExposed : Tron msg -> Tron ( ( LabelPath, String ), msg )
toStrExposed =
    toProxied
        >> Property.addLabeledPath
        >> Property.map
            (\( path, ( proxyVal, msg ) ) ->
                ( ( path
                  , ProxyValue.toString proxyVal
                  )
                , msg
                )
            )


reflect : Property a -> Property ( ProxyValue, a )
reflect prop =
    case prop of
        Nil -> Nil
        Number control ->
            Number
                <| Control.map (Tuple.mapFirst Tuple.second >> Tuple.mapFirst FromSlider)
                <| Control.reflect
                <| control
        Coordinate control ->
            Coordinate
                <| Control.map (Tuple.mapFirst Tuple.second >> Tuple.mapFirst FromXY)
                <| Control.reflect
                <| control
        Text control ->
            Text
                <| Control.map (Tuple.mapFirst Tuple.second >> Tuple.mapFirst FromInput)
                <| Control.reflect
                <| control
        Color control ->
            Color
                <| Control.map (Tuple.mapFirst Tuple.second >> Tuple.mapFirst FromColor)
                <| Control.reflect
                <| control
        Toggle control ->
            Toggle
                <| Control.map (Tuple.mapFirst FromToggle)
                <| Control.reflect
                <| control
        Action control ->
            Action
                <| Control.map (Tuple.mapFirst <| always FromButton)
                <| Control.reflect
                <| control
        Choice focus shape control ->
            Choice focus shape
                <| Nest.mapItems (Tuple.mapSecond reflect)
                <| Control.map (Tuple.mapFirst <| .selected >> FromChoice)
                <| Control.reflect
                <| control
        Group focus shape control ->
            Group focus shape
                <| Nest.mapItems (Tuple.mapSecond reflect)
                <| Control.map (Tuple.mapFirst <| always Other)
                <| Control.reflect
                <| control


lift : Property a -> Property (ProxyValue -> Maybe a)
lift =
    Property.map (always << Just)


evaluate : Property (ProxyValue -> Maybe a) -> Property (Maybe a)
evaluate =
    reflect
    >> Property.map (\(v, handler) -> handler v)


