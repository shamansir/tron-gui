module Tron.Option.Render exposing
    ( Target(..)
    , hidden, toHtml, toDebug, toVr
    )


{-| The configuration of where to render Tron interface.

@docs Target, hidden, toHtml, toVr, toDebug
-}


import Tron.Style.Theme exposing (Theme(..))
import Tron.Style.Dock exposing (Dock(..))
import Tron.Msg exposing (Msg_(..))


{-| Where to render the GUI:

- Nowhere, the GUI is hidden;
- To HTML, with given Dock and Theme;
- To AFrame (VR), with given Theme (experimental);

-}
type Target
    = Html Dock Theme
    | Aframe Theme
    | Debug Dock Theme
    | Nowhere


{-| GUI is hidden. For example, for the case of `dat.gui`, where your interface is on the JS side, but uses Tron definition in Elm.

See `example/DatGui` for details.
-}
hidden : Target
hidden = Nowhere


{-| Render to HTML using given theme (dark/light) and docked at the requested side (see `Tron.Style.Dock`). Most used option!
-}
toHtml : Dock -> Theme -> Target
toHtml = Html


{-| Render to Debug mode where all the controls are represented as text boxes with information.
-}
toDebug : Dock -> Theme -> Target
toDebug = Debug


{-| Render to Virtual Reality using given theme (dark/light); Experimental. Uses `a-frame` library for render, so it should be included in your HTML;

See `example/AFrame` for details.
-}
toVr : Theme -> Target
toVr = Aframe
