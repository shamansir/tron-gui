module Tron.Style.Theme exposing
    ( Theme(..)
    , dark, light
    , toggle
    , toString
    )

{-| # Theme
@docs Theme

# Values
@docs dark, light

# Toggling
@docs toggle

# Name
@docs toString
-}


{-| Dark or Light theme, each of those could be useful for different situation.
-}
type Theme
    = Dark
    | Light


{-| Toggle theme from Light to Dark and vice-versa
-}
toggle : Theme -> Theme
toggle from =
    case from of
        Dark -> Light
        Light -> Dark


{-|
-}
dark : Theme
dark = Dark


{-|
-}
light : Theme
light = Light



-- TODO: `label`?
{-| -}
toString : Theme -> String
toString theme =
    case theme of
        Dark -> "dark"
        Light -> "light"
