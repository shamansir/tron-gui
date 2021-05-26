module Tron.Control.XY exposing (..)


import Axis exposing (Axis)

import Tron.Control as Core exposing (Control)


-- if the XY is being dragged now, we need to know its first value it had when user started dragging,
-- so it is the first `Maybe` in the pair
type alias Control a =
    Core.Control
        ( Axis, Axis )
        ( Maybe ( Float, Float ), ( Float, Float ) )
        a


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

