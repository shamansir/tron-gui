module Tron.Control.GenUI.Toggle exposing (to, from)


import GenUI


import Tron.Control as Core
import Tron.Control.Impl.Toggle as Toggle exposing (Control)


to : Control a -> GenUI.Def x
to (Core.Control _ current _) =
    GenUI.Toggle
        { current = Toggle.toggleToBool current
        }


from : GenUI.Def x -> Maybe (Control ())
from def =
    case def of
        GenUI.Toggle toggleDef ->
            Just <|
                Core.Control
                    ()
                    (Toggle.boolToToggle toggleDef.current)
                    ()
        _ -> Nothing