module Tron.Control.GenUI.Button exposing (to, from)


import GenUI
import Color


import Tron.Control as Core
import Tron.Control.Impl.Button as Button exposing (Control)
import Tron.Style.Theme as Theme


to : Control a -> GenUI.Def
to (Core.Control face _ _) =
    GenUI.Action
        { face =
            case face of
                Button.Default -> GenUI.Default
                Button.WithIcon icon -> GenUI.Icon <| Maybe.withDefault "" <| Maybe.map Button.maybeLocalUrlToString <| icon Theme.Light
                Button.WithColor color -> GenUI.OfColor <| Color.toCssString color
        }



from : GenUI.Def -> Maybe (Control ())
from def =
    case def of
        Action actionDef ->
            Just <|
                Core.Control
                    (case actionDef.face of
                        GenUI.Default -> Button.Default
                        GenUI.Icon icon -> Button.WithIcon <| always <| Just <| Button.toLocalUrl_ icon
                        GenUI.OfColor colorStr -> Button.WithColor <| Color.darkBlue -- FIXME
                    )
                    ()
                    ()
        _ -> Nothing