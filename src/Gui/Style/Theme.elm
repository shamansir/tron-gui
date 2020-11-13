module Gui.Style.Theme exposing
    ( Theme
    , dark, light
    )

{-| # Theme
@docs Theme

# Values
@docs dark, light
-}


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
