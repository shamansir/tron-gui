module Tron.Control.GenUI.Color exposing (to, from, gColorToColor, colorToGColor)

import GenUI
import GenUI.Color as GColor
import Color as Color exposing (Color)
import Color.Extra as Color


import Tron.Control as Core
import Tron.Control.Impl.Color as Color exposing (Control)

gColorToColor : GColor.Color -> Color
gColorToColor c =
    case c of
        GColor.Rgba rgba -> Color.fromRgba rgba
        GColor.Hsla hsla -> Color.fromHsla hsla
        GColor.Hex hex -> Color.fromHex hex |> Maybe.withDefault Color.black


colorToGColor : Color -> GColor.Color
colorToGColor = Color.toRgba >> GColor.Rgba


to : Control a -> GenUI.Def
to (Core.Control _ (_, current ) _) =
    GenUI.Color
        { current = colorToGColor current
        }



from : GenUI.Def -> Maybe (Control ())
from def =
    case def of
        GenUI.Color colorDef ->
            Just <|
                Core.Control
                    ()
                    (Nothing, gColorToColor colorDef.current)
                    ()
        _ -> Nothing