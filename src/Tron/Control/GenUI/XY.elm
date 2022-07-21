module Tron.Control.GenUI.XY exposing (to, from)


import GenUI


import Tron.Control as Core
import Tron.Control.Impl.XY as XY exposing (Control)


to : Control a -> GenUI.Def x
to (Core.Control ( xAxis, yAxis ) (_, ( currentX, currentY ) ) _) =
    GenUI.XY
        { x =
            { min = xAxis.min
            , max = xAxis.max
            , step = xAxis.step
            , current = currentX
            }
        , y =
            { min = yAxis.min
            , max = yAxis.max
            , step = yAxis.step
            , current = currentY
            }
        }



from : GenUI.Def x -> Maybe (Control ())
from def =
    case def of
        GenUI.XY xyDef ->
            Just <|
                Core.Control
                    ( { min = xyDef.x.min, max = xyDef.x.max, step = xyDef.x.step }
                    , { min = xyDef.y.min, max = xyDef.y.max, step = xyDef.y.step }
                    )
                    ( Nothing, ( xyDef.x.current, xyDef.y.current) )
                    ()
        _ -> Nothing
