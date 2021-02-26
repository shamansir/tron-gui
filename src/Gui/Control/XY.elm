module Gui.Control.XY exposing (..)


import Axis exposing (Axis)

import Gui.Control as Core exposing (Control)


type alias Control msg = Core.Control ( Axis, Axis ) ( Float, Float ) msg


xyToString : ( Float, Float ) -> String
xyToString (x, y) = String.fromFloat x ++ "|" ++ String.fromFloat y


xyFromString : String -> Maybe ( Float, Float )
xyFromString str =
    case String.split "|" str of
        xStr::yStr::_ ->
            Maybe.map2 Tuple.pair (String.toFloat xStr) (String.toFloat yStr)
        _ -> Nothing

