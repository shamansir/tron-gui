module Gui.Style.Theme exposing
    ( Theme
    , dark, light
    , switch
    , back, lines, secondaryLines, text, textHilite
    , focusBack
    )

{-| # Theme
@docs Theme

# Values
@docs dark, light

# Colors
@docs lines, back

# Helpers
@docs switch
-}

import Color
import Color exposing (Color)
import Gui.Focus exposing (Focused(..))
import Gui.Style.Placement exposing (Placement(..))
import Gui.Style.Selected exposing (Selected(..))


transparent = Color.rgba 0.0 0.0 0.0 0.0


{-| Dark or Light theme, each of those could be useful for different situation.
-}
type Theme
    = Dark
    | Light


{-|
-}
dark : Theme
dark = Dark


{-|
-}
light : Theme
light = Light


{-| Toggle theme from Light to Dark and vice-versa
-}
switch : Theme -> Theme
switch from =
    case from of
        Dark -> Light
        Light -> Dark


{-|
-}
lines : Theme -> Color
lines theme =
    case theme of
        Dark -> Color.white
        Light -> Color.rgb255 58 62 65 -- "#3a3e41"


{-|
-}
secondaryLines : Theme -> Color
secondaryLines theme =
    case theme of
        Dark -> Color.rgba 1.0 1.0 1.0 0.15
        Light -> Color.rgb255 220 220 220 -- "#eeeeee"


{-|
-}
back : Theme -> Placement -> Color
back theme placement =
    case placement of
        OnAPlate -> transparent
        AtRoot ->
            case theme of
                Dark -> Color.rgba 0.05 0.05 0.05 0.6
                Light -> Color.rgba 1.0 1.0 1.0 0.8


{-|
-}
text : Theme -> Color
text _ =
    Color.rgb255 144 144 144


{-|
-}
textHilite : Theme -> Color
textHilite theme =
    case theme of
        Dark -> Color.white
        Light -> Color.black


{-
focusLines : Theme -> Focused -> Color
focusLines theme =
    always <| lines theme


focusSecondaryLines : Theme -> Focused -> Color
focusSecondaryLines theme =
    always <| secondaryLines theme
-}


{-|
-}
focusBack : Theme -> Placement -> Focused -> Color
focusBack theme placement focused =
    case focused of
        NotFocused ->
            back theme placement
        FocusedBy n ->
            case theme of
                Light ->
                    Color.rgb 1.0 1.0 1.0
                Dark ->
                    Color.rgb 0.2 0.2 0.2
                {-
                if n == 0 then rgb 1.0 1.0 1.0
                else if n == 1 then rgb 0.95 0.95 0.95
                else ... -}
