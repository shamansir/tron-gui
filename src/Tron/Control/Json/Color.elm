module Tron.Control.Json.Color exposing (..)


import Color as Color exposing (Color)
import Color.Convert as Color

import Tron.Control as Core
import Tron.Control.Impl.Color exposing (Control)

import Json.Decode as D
import Json.Encode as E


decode : D.Decoder (Control ())
decode =
    D.field "currentRgba" decodeColor
        |> D.map
            (\current ->
                Core.Control
                    ()
                    ( Nothing, current )
                    ()
            )


encode : Control a -> List ( String, E.Value )
encode (Core.Control _ ( _, val ) _) =
    [ ( "current", encodeColor val )
    , ( "currentRgba", encodeRgba val )
    ]


encodeColor : Color -> E.Value
encodeColor =
    E.string << Color.colorToHexWithAlpha


encodeRgba : Color -> E.Value
encodeRgba color =
    case Color.toRgba color of
        { red, green, blue, alpha } ->
            E.object
                [ ( "red", E.float red )
                , ( "green", E.float green )
                , ( "blue", E.float blue )
                , ( "alpha", E.float alpha )
                ]


decodeColor : D.Decoder Color
decodeColor =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    str
                        |> Color.hexToColor
                        |> Result.map D.succeed
                        |> Result.withDefault (D.fail <| "failed to parse color: " ++ str)
                )
        -- FIXME: use either one in the corresponding place
        , D.map4
            Color.rgba
            (D.field "red" D.float)
            (D.field "green" D.float)
            (D.field "blue" D.float)
            (D.map (Maybe.withDefault 1.0) <| D.maybe <| D.field "alpha" D.float)
        ]
