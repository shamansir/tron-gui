module Gui.Style.Flow exposing
    ( Flow
    , topToBottom, bottomToTop, leftToRight, rightToLeft
    )

{-| # Flow
@docs Flow

# Values
@docs topToBottom, bottomToTop, leftToRight, rightToLeft
-}


{-| Flow describes the direction in which GUI is oriented and to which side it is "docked".

If you are familiar with macOS Dock — here we have the similar concept.
-}
type Flow
    = TopToBottom
    | BottomToTop
    | LeftToRight
    | RightToLeft


{-|
-}
topToBottom : Flow
topToBottom = TopToBottom


{-|
-}
bottomToTop : Flow
bottomToTop = BottomToTop


{-|
-}
leftToRight : Flow
leftToRight = LeftToRight


{-|
-}
rightToLeft : Flow
rightToLeft = RightToLeft
