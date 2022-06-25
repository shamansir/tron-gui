module Tron.Control.GenUI.Button exposing (to, from, faceTo, faceFrom)


import GenUI
import Color


import Tron.Control as Core
import Tron.Control.Impl.Button as Button exposing (Control)
import Tron.Style.Theme as Theme


faceTo : Button.Face -> GenUI.Face
faceTo face =
    case face of
        Button.Default -> GenUI.Default
        Button.WithIcon (Button.Icon iconFn) -> GenUI.Icon <| Maybe.withDefault "" <| Maybe.map Button.maybeLocalUrlToString <| iconFn Theme.Light
        Button.WithColor color -> GenUI.OfColor <| Color.toCssString color


faceFrom : GenUI.Face -> Button.Face
faceFrom face =
    case face of
        GenUI.Default -> Button.Default
        GenUI.Icon icon -> Button.WithIcon <| Button.Icon <| always <| Just <| Button.toLocalUrl_ icon
        GenUI.OfColor colorStr -> Button.WithColor <| Color.darkBlue -- FIXME


to : Control a -> GenUI.Def
to (Core.Control face _ _) =
    GenUI.Action
        { face = faceTo face
        }



from : GenUI.Def -> Maybe (Control ())
from def =
    case def of
        GenUI.Action actionDef ->
            Just <|
                Core.Control
                    (faceFrom actionDef.face)
                    ()
                    ()
        _ -> Nothing