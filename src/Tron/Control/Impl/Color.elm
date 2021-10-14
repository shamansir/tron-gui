module Tron.Control.Impl.Color exposing (..)


import Color exposing (Color)

import Tron.Control as Core exposing (Control)
import Tron.Control.Action as A
import Tron.Util as U


-- if the color component is being dragged now, we need to know its first value it had when user started dragging,
-- so it is the first `Maybe` in the pair
type alias Control a = Core.Control () (Maybe Color, Color) a


update : A.Action -> Control a -> ( Control a, A.Change )
update action ( Core.Control axis ( maybeFromColor, curColor ) a) =
    let
        hueAxis = { min = 0, max = 1, step = 0.01 }
        lgtAxis = { min = 0, max = 1, step = 0.01 }
        curHsla = Color.toHsla curColor
    in

    ( Core.Control
        axis
        (
            case action of

                A.DragStart ->
                    ( Just curColor
                    , curColor
                    )

                A.Dragging { dx, dy } ->
                    ( maybeFromColor
                    ,
                        let
                            ( hueToAlter, lightnessToAlter ) =
                                case maybeFromColor of
                                    Just fromColor ->
                                        case Color.toHsla fromColor of
                                            hsla -> ( hsla.hue, hsla.lightness )
                                    Nothing -> ( curHsla.hue, curHsla.lightness )
                        in
                            Color.hsla
                                (U.alter hueAxis dx hueToAlter)
                                (if curHsla.saturation > 0.25 then
                                    -- && curHsla.saturation < 0.75 then
                                        curHsla.saturation
                                else 0.5)
                                (U.alter lgtAxis dy lightnessToAlter)
                                curHsla.alpha
                    )

                A.DragFinish ->
                    ( Nothing
                    , curColor
                    )

                _ -> ( maybeFromColor, curColor )
        )
        a
    , case action of
        A.DragStart -> A.Silent
        A.Dragging _ -> A.Silent
        A.DragFinish -> A.Fire
        _ -> A.None
    )