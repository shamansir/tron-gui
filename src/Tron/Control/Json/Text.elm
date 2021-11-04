module Tron.Control.Json.Text exposing (..)


import Tron.Control as Core
import Tron.Control.Impl.Text as Text exposing (Control)

import Json.Decode as D
import Json.Encode as E


decode : D.Decoder (Control ())
decode =
    D.field "current" D.string
        |> D.map
            (\current ->
                Core.Control
                    ()
                    ( Text.Ready, current )
                    ()
            )


encode : Control a -> List ( String, E.Value )
encode (Core.Control _ ( _, val ) _) =
    [ ( "current", E.string val )
    ]