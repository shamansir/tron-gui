module Tron.Control.Impl.XY exposing (..)


import Axis exposing (Axis)

import Tron.Control as Core exposing (Control)
import Tron.Control.Action as A
import Tron.Util as U


import Json.Decode as D
import Json.Encode as E


-- if the XY is being dragged now, we need to know its first value it had when user started dragging,
-- so it is the first `Maybe` in the pair
type alias Control a =
    Core.Control
        ( Axis, Axis )
        ( Maybe ( Float, Float ), ( Float, Float ) )
        a


update : A.Action -> Control a -> ( Control a, A.Change )
update action ( Core.Control ( xAxis, yAxis ) ( maybeFrom, ( curX, curY ) ) a) =
    ( Core.Control
        ( xAxis, yAxis )
        (
            case action of

                A.DragStart ->
                    ( Just ( curX, curY )
                    , ( curX, curY )
                    )

                A.Dragging { dx, dy } ->
                    let
                        ( xToAlter, yToAlter ) =
                            maybeFrom |> Maybe.withDefault ( curX, curY )
                        ( nextX, nextY ) =
                            ( U.alter xAxis dx xToAlter
                            , U.alter yAxis (1.0 - dy) yToAlter
                            )
                    in
                        ( maybeFrom, ( nextX, nextY ) )

                A.DragFinish ->
                    ( Nothing, ( curX, curY ) )

                _ -> ( maybeFrom, ( curX, curY ) )
        )
        a
    , case action of
        A.DragStart -> A.Silent
        A.Dragging _ -> A.Silent
        A.DragFinish -> A.Fire
        _ -> A.Stay
    )


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