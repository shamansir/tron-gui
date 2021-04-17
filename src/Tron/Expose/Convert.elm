module Tron.Expose.Convert exposing (..)


import Json.Encode as E

import Tron.Path as Path
import Tron.Expose.Data exposing (..)
import Tron.Property exposing (..)
import Tron.ProxyValue as ProxyValue exposing (ProxyValue(..))
import Tron.Control exposing (mapWithValue)
import Tron.Control.Nest as Nest


getTypeString :
    ProxyValue
    -> String -- FIXME: move to ProxyValue
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



toProxied : Property msg -> Property ( ProxyValue, msg )
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


-- FIXME: make it: Property msg -> Property RawOutUpdate and use `Property.map2` to join
toExposed : Property msg -> Property ( RawOutUpdate, msg )
toExposed prop =
    prop
        |> toProxied
        |> Tron.Property.addPaths
        -- FIXME: `Expose.encodeUpdate` does the same as above
        |> Tron.Property.map
            (\( ( path, labelPath ), ( proxyVal, msg ) ) ->
                ( { path = Path.toList path
                  , labelPath = labelPath
                  , type_ = getTypeString proxyVal
                  , value = ProxyValue.encode proxyVal
                  , stringValue = ProxyValue.toString proxyVal
                  , client = E.null -- FIXME: store clientId separately in `Detach`
                  }
                , msg
                )
            )


-- FIXME: make it: Property msg -> Property ( LabelPath, String ) and use `Property.map2` to join
toStrExposed : Property msg -> Property ( ( LabelPath, String ), msg )
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

