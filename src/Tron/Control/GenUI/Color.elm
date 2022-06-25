module Tron.Control.GenUI.Color exposing (to, from)

import GenUI
import Color


import Tron.Control as Core
import Tron.Control.Impl.Color as Color exposing (Control)


to : Control a -> GenUI.Def
to (Core.Control _ (_, current ) _) =
    GenUI.Color
        { current = Color.toCssString current
        }



from : GenUI.Def -> Maybe (Control ())
from def =
    case def of
        GenUI.Color colorDef ->
            Just <|
                Core.Control
                    ()
                    (Nothing, Color.blue) -- FIXME
                    ()
        _ -> Nothing