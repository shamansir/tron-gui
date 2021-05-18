module Tron.Control.XY exposing (..)


import Axis exposing (Axis)

import Tron.Control as Core exposing (Control)


type alias Control a = Core.Control ( Axis, Axis ) ( Float, Float ) a


separator : String
separator = ";"


xyToString : ( Float, Float ) -> String
xyToString (x, y) = String.fromFloat x ++ separator ++ String.fromFloat y


xyFromString : String -> Maybe ( Float, Float )
xyFromString str =
    case String.split separator str of
        xStr::yStr::_ ->
            Maybe.map2 Tuple.pair (String.toFloat xStr) (String.toFloat yStr)
        _ -> Nothing

