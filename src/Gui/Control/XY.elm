module Gui.Control.XY exposing (..)


import Axis exposing (Axis)

import Gui.Control as Core exposing (Control)


type alias Control msg = Core.Control ( Axis, Axis ) ( Float, Float ) msg


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

