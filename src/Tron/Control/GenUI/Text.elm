module Tron.Control.GenUI.Text exposing (to, from)


import GenUI


import Tron.Control as Core
import Tron.Control.Impl.Text as Text exposing (Control)


to : Control a -> GenUI.Def x
to (Core.Control _ (_, current ) _) =
    GenUI.Textual
        { current = current
        }


from : GenUI.Def x -> Maybe (Control ())
from def =
    case def of
        GenUI.Textual textDef ->
            Just <|
                Core.Control
                    ()
                    ( Text.Ready, textDef.current )
                    ()
        _ -> Nothing