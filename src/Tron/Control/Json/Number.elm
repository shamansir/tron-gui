module Tron.Control.Json.Number exposing (..)


import Tron.Control as Core
import Tron.Control.Impl.Number exposing (Control)

import Json.Decode as D
import Json.Encode as E


decode : D.Decoder (Control ())
decode =
    D.map4
        (\min max step current ->
            Core.Control
                { min = min
                , max = max
                , step = step
                }
                ( Nothing, current )
                ()
        )
        (D.field "min" D.float)
        (D.field "max" D.float)
        (D.field "step" D.float)
        (D.field "current" D.float)


encode : Control a -> List ( String, E.Value )
encode (Core.Control { min, max, step } ( _, val ) _) =
    [ ( "current", E.float val )
    , ( "min", E.float min )
    , ( "max", E.float max )
    , ( "step", E.float step )
    ]