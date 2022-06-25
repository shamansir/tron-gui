module Tron.Control.GenUI.Number exposing (to, from)


import GenUI


import Tron.Control as Core
import Tron.Control.Impl.Number as Number exposing (Control)


to : Control a -> GenUI.Def
to (Core.Control axis (_, current ) _) =
    GenUI.NumFloat
        { min = axis.min
        , max = axis.max
        , step = axis.step
        , current = current
        }


from : GenUI.Def -> Maybe (Control ())
from def =
    case def of
        GenUI.NumInt numDef ->
            Just <|
                Core.Control
                    { min = toFloat numDef.min, max = toFloat numDef.max, step = toFloat numDef.step }
                    ( Nothing, toFloat numDef.current )
                    ()
        GenUI.NumFloat numDef ->
            Just <|
                Core.Control
                    { min = numDef.min, max = numDef.max, step = numDef.step }
                    ( Nothing, numDef.current )
                    ()
        _ -> Nothing