module Tron.Expose.Convert exposing (toExposed, toProxied, toStrExposed, toUnit)


{-| Make your `Tron *` store the additional information along with messages,
or just get rid of messages at all:

@docs toUnit, toProxied, toExposed, toStrExposed
-}


import Json.Encode as E

import Tron exposing (Tron)
import Tron.Control exposing (mapWithValue)
import Tron.Control.Nest as Nest
import Tron.Expose.Data exposing (..)
import Tron.Path as Path
import Tron.Property exposing (..)
import Tron.Expose.ProxyValue as ProxyValue exposing (ProxyValue(..))


{-| Instead of messages, store nothing, but values.
-}
toUnit : Tron msg -> Tron ()
toUnit =
    map <| always ()


{-| Store a `ProxyValue` together with message, which mirrors a value
as a data type to make it easier to connect with other APIs, where message is not
important, such as `dat.gui`, or send it to ports along with sending it to user.

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
toProxied : Tron msg -> Tron ( ProxyValue, msg )
toProxied prop =
    let
        helper : (v -> ProxyValue) -> (v -> msg -> ( ProxyValue, msg ))
        -- helper : (a -> b) -> ( a -> c -> ( b, c ) )
        helper toProxy v msg =
            ( toProxy v, msg )
    in
    case prop of
        Nil ->
            Nil

        Number control ->
            control
                |> mapWithValue (helper FromSlider)
                |> Number

        Coordinate control ->
            control
                |> mapWithValue (helper FromXY)
                |> Coordinate

        Text control ->
            control
                |> mapWithValue (helper (Tuple.second >> FromInput))
                |> Text

        Color control ->
            control
                |> mapWithValue (helper FromColor)
                |> Color

        Toggle control ->
            control
                |> mapWithValue (helper FromToggle)
                |> Toggle

        Action control ->
            control
                |> mapWithValue (helper <| always FromButton)
                |> Action

        Choice focus shape control ->
            control
                |> Nest.mapItems (Tuple.mapSecond toProxied)
                |> mapWithValue (helper (.selected >> FromChoice))
                |> Choice focus shape

        Group focus shape control ->
            control
                |> Nest.mapItems (Tuple.mapSecond toProxied)
                |> mapWithValue (helper <| always Other)
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
toExposed : Tron msg -> Tron ( RawOutUpdate, msg )
toExposed prop =
    prop
        |> toProxied
        |> Tron.Property.addPaths
        -- FIXME: `Expose.encodeUpdate` does the same as above
        |> Tron.Property.map
            (\( ( path, labelPath ), ( proxyVal, msg ) ) ->
                ( { path = Path.toList path
                  , labelPath = labelPath
                  , type_ = ProxyValue.getTypeString proxyVal
                  , value = ProxyValue.encode proxyVal
                  , stringValue = ProxyValue.toString proxyVal
                  , client = E.null -- FIXME: store clientId separately in `Detach`
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
toStrExposed prop =
    prop
        |> toProxied
        |> Tron.Property.addLabeledPath
        |> Tron.Property.map
            (\( path, ( proxyVal, msg ) ) ->
                ( ( path
                  , ProxyValue.toString proxyVal
                  )
                , msg
                )
            )
