module Tron.Control.Impl.Number exposing (..)


import Axis exposing (Axis)

import Tron.Util as U
import Tron.Control as Core exposing (Control)
import Tron.Control.Action as A

-- if the slider is being dragged now, we need to know its first value it had when user started dragging,
-- so it is the first `Maybe` in the pair
type alias Control a = Core.Control Axis ( Maybe Float, Float ) a


update : A.Action -> Control a -> ( Control a, A.Change )
update action ( Core.Control axis ( maybeFrom, curValue ) a) =
    ( Core.Control
        axis
        (
            case action of

                A.DragStart ->
                    ( Just curValue
                    , curValue
                    )

                A.Dragging { dy } ->
                    ( maybeFrom
                    , maybeFrom
                        |> Maybe.withDefault curValue
                        |> U.alter axis dy
                    )

                A.DragFinish ->
                    ( Nothing
                    , curValue
                    )

                _ -> ( maybeFrom, curValue )
        )
        a
    , case action of
        A.DragStart -> A.Silent
        A.Dragging _ -> A.Silent
        A.DragFinish -> A.Fire
        _ -> A.Stay
    )