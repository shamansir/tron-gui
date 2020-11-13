module Gui.Style.Theme exposing
    ( Theme
    , dark, light
    , switch
    , back, lines, secondaryLines, text
    , toString
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
lines : Theme -> ( Placement, Focused, Selected ) -> Color
lines theme _ =
    case theme of
        Dark -> Color.white
        Light -> Color.rgb255 58 62 65 -- "#3a3e41"


{-|
-}
secondaryLines : Theme -> ( Placement, Focused, Selected ) -> Color
secondaryLines theme _ =
    case theme of
        Dark -> Color.rgba 1.0 1.0 1.0 0.15
        Light -> Color.rgb255 220 220 220 -- "#eeeeee"


{-|
-}
back : Theme -> ( Placement, Focused, Selected ) -> Color
back theme ( placement, focused, _ ) =
    case placement of
        OnAPlate -> transparent
        AtRoot ->
            case ( theme, focused ) of
                ( Dark, NotFocused ) -> Color.rgba 0.05 0.05 0.05 0.6
                ( Light, NotFocused ) -> Color.rgba 1.0 1.0 1.0 0.8
                ( Dark, FocusedBy _ ) -> Color.rgb 0.2 0.2 0.2
                ( Light, FocusedBy _ ) -> Color.rgb 1.0 1.0 1.0


{-|
-}
text : Theme -> ( Placement, Focused, Selected ) -> Color
text theme ( _, _, selected ) =
    case selected of
        Usual -> Color.rgb255 144 144 144
        Selected ->
            case theme of
                Dark -> Color.white
                Light -> Color.black


-- TODO: `label`?

toString : Theme -> String
toString theme =
    case theme of
        Dark -> "dark"
        Light -> "light"
