module Tron.Control.Json.Toggle exposing (..)

import Tron.Control as Core
import Tron.Control.Impl.Toggle as Toggle exposing (Control)

import Json.Decode as D
import Json.Encode as E


decode : D.Decoder (Control ())
decode =
    D.field "current" decodeToggle
        |> D.map
            (\current ->
                Core.Control
                    ()
                    current
                    ()
            )


encode : Control a -> List ( String, E.Value )
encode (Core.Control _ val _) =
    [
        ( "current"
        , E.string <|
            case val of
                Toggle.TurnedOn ->
                    "on"

                Toggle.TurnedOff ->
                    "off"
        )
    ]


decodeToggle : D.Decoder Toggle.ToggleState
decodeToggle =
    D.oneOf
        [ D.bool
            |> D.map
                (\bool ->
                    if bool then Toggle.TurnedOn else Toggle.TurnedOff
                )
        -- FIXME: use either one in the corresponding place
        , D.string
            |> D.map (\str ->
                case str of
                    "on" ->
                        Toggle.TurnedOn

                    "off" ->
                        Toggle.TurnedOff

                    _ ->
                        Toggle.TurnedOff
            )
        ]