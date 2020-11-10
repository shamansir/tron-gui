module Gui.Render.Style exposing
    ( Flow(..), CellShape(..), Theme(..) )


{-| Style module defines the ways to configure the GUI look

# Flow
@docs Flow

# Theme
@docs Theme

# Cell Shape
@docs CellShape
-}


{-| Flow describes the direction in which GUI is oriented and to which side it is "docked".

If you are familiar with macOS Dock — here we have the similar concept.
-}
type Flow
    = TopToBottom
    | BottomToTop
    | LeftToRight
    | RightToLeft


{-| Dark or Light theme, each of those could be useful for different situation.
-}
type Theme
    = Dark
    | Light


{-| Cell Shape is the place it takes in nested panels. Considering the default shape as 1x1 (`Full`), the meaning of each value is:

* `Full` — 1x1
* `Half` — 0.5x0.5
* `HalfByOne` — 0.5x1
* `OneByHalf` — 1x0.5
* `TwiceByHalf` — 2x0.5
* `HalfByTwice` — 0.5x2
* `TwiceByTwice` - 2x2
-}
type CellShape
    = Full
    | Half
    | HalfByOne
    | OneByHalf
    | TwiceByHalf
    | HalfByTwice
    | TwiceByTwice
