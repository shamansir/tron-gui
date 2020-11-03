module Gui.Render.Style exposing
    ( Flow(..), CellShape(..), Theme(..) )


{-| Style module defines the ways to configure the GUI look

# Flow
@docs Flow

# Theme
@docs Theme
-}


{-| Flow describes the direction in which GUI is oriented and to which side it is "docked".

If you are familiar with macOS Dock — here we have the similar concept.
-}
type Flow
    = TopToBottom
    | BottomToTop
    | LeftToRight
    | RightToLeft


type CellShape
    = Full_Full
    | Half_Half
    | Half_Full
    | Full_Half
    | Twice_Half
    | Half_Twice


{-| Dark or Light theme, each of those could be useful for different situation.
-}
type Theme
    = Dark
    | Light
