module Tron.Expose.Convert
    exposing
    ( toUnit, toExposed, toProxied, toStrExposed
    , reflect, lift, evaluate
    , toDeferredRaw, encodeAck
    )


{-| Make your `Tron *` store the additional information along with messages,
or just get rid of messages at all:

@docs toUnit, toProxied, toExposed, toStrExposed

# Operations on the value

@docs reflect, lift, evaluate

# Internal

@docs toDeferredRaw, encodeAck
-}


import Json.Encode as E
import Dict exposing (Dict)
import HashId exposing (HashId)

import Tron exposing (Tron)
import Tron.OfValue as Def
import Tron.Control as Control
import Tron.Control.Nest as Nest
import Tron.Expose.Data as Exp
import Tron.Path as Path exposing (Path)
import Tron.Property as Property exposing (..)
import Tron.Control.Value as Value exposing (Value(..))


{-| Replace the `()` value everywhere within Tron GUI tree, it is useful for truly a lot of cases when you don't care about what are the associated values.
-}
toUnit : Tron a -> Tron ()
toUnit = Tron.toUnit


{-| Store a `Value` together with message, which mirrors a value
as a data type to make it easier to connect with other APIs, where message is not
important, such as `dat.gui`, or send it to ports along with sending it to user.

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
toProxied : Tron a -> Tron ( Value, a )
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
                |> convertWith (always FromGroup)
                -- TODO: notify expanded/collapsed/detached?
                |> Group focus shape

        Live innerProp ->
            Live <| toProxied innerProp



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
toExposed : Tron a -> Tron ( Exp.Value, a )
toExposed =
    toProxied
        >> Property.addPaths
        -- FIXME: `Expose.encodeUpdate` does the same as above
        >> Property.map
            (\( path, ( proxyVal, msg ) ) ->
                ( proxyVal |> proxyToRaw path
                , msg
                )
            )


proxyToRaw : ( Path, LabelPath ) -> Value -> Exp.Value
proxyToRaw (path, labelPath) proxyVal =
    { path = Path.toList path
    , labelPath = labelPath
    , type_ = Value.getTypeString proxyVal
    , value = Value.encode proxyVal
    , stringValue = Value.toString proxyVal
    }



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
                  , Value.toString proxyVal
                  )
                , msg
                )
            )


{-| Extract the value from the control and put it along with a subject of the functor.
-}
reflect : Property a -> Property ( Value, a )
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
                <| Control.map (Tuple.mapFirst <| always FromGroup)
                <| Control.reflect
                <| control
        Live innerProp ->
            Live
                <| reflect innerProp


{-| Create proxied property. Notice that It will return the `a` disregardless of what `Value` is. -}
lift : Property a -> Property (Value -> Maybe a)
lift =
    Property.map (always << Just)



{-| Use current value of the control and apply it to the handler. -}
evaluate : Property (Value -> Maybe a) -> Property (Maybe a)
evaluate =
    reflect
    >> Property.map (\(v, handler) -> handler v)


{-| -}
toDeferredRaw : Property a -> Property (Value -> Maybe Exp.Value)
toDeferredRaw =
    Property.addPaths
        >> Property.map
            (\(path, _) ->
                Just << proxyToRaw path
            )


-- {-| -}
-- encodeClientId : HashId -> E.Value
-- encodeClientId =
--     HashId.toString >> E.string


{-| -}
encodeAck : HashId -> Exp.Ack
encodeAck =
    -- encodeClientId
    Exp.Ack << (HashId.toString >> E.string)