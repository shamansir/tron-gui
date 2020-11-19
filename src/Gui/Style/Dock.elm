module Gui.Style.Dock exposing
    ( Dock
    , topToBottom, bottomToTop, leftToRight, rightToLeft
    , adaptBounds, adaptPosition, adaptSize, firstCellAt
    , boundsFromSize
    , toString
    )

{-| # Dock
@docs Dock

# Values
@docs topToBottom, bottomToTop, leftToRight, rightToLeft
-}

import Gui.Style.Cell as Cell

import Size exposing (..)


type HorzAnchor
    = Left
    | Center
    | Right


type VertAnchor
    = Top
    | Middle
    | Bottom


{-| `Dock` describes the direction in which GUI is oriented and to which corner it is "docked".

If you are familiar with macOS Dock — here we have the similar concept.
-}
type Dock = Dock ( HorzAnchor, VertAnchor )

{-|
-}
topLeft : Dock
topLeft = Dock ( Left, Top )


{-|
-}
topCenter : Dock
topCenter = Dock ( Center, Top )


{-|
-}
topRight : Dock
topRight = Dock ( Right, Top )


{-|
-}
middleLeft : Dock
middleLeft = Dock ( Middle, Left )


{-|
-}
center : Dock
center = Dock ( Middle, Center )


{-|
-}
middleRight : Dock
middleRight = Dock ( Middle, Right )


{-|
-}
bottomLeft : Dock
bottomLeft = Dock ( Bottom, Left )


{-|
-}
bottomCenter : Dock
bottomCenter = Dock ( Bottom, Center )


{-|
-}
bottomRight : Dock
bottomRight = Dock ( Bottom, Right )


adaptBounds
     : Dock
    -> ( Float, Float )
    -> { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
adaptBounds (Dock ( horz, vert )) ( width, height ) innerBounds =
    { width =
        case horz of
            Left ->
    , height =
    }

    case dock of
        ( Bottom, _ ) ->
            { innerBounds
            | y = height - innerBounds.y - innerBounds.height
            }
        ( _, Right ) ->
            { width = innerBounds.height
            , height = innerBounds.width
            , x = innerBounds.y
            , y = innerBounds.x
            }
        {- RightToLeft ->
            { width = innerBounds.height
            , height = innerBounds.width
            , x = height - innerBounds.y - innerBounds.height
            , y = innerBounds.x
            } -}
        _ -> innerBounds


adaptPosition : Dock -> ( Float, Float ) -> { x : Float, y : Float } -> { x : Float, y : Float }
adaptPosition dock ( width, height ) { x, y } =
    case dock of
        TopToBottom -> { x = x, y = y }
        BottomToTop ->
            { x = x
            , y = height - y
            }
        LeftToRight ->
            { x = y
            , y = x
            }
        RightToLeft ->
            { x = y
            , y = width - x
            }


adaptSize : Dock -> ( Float, Float ) -> ( Float, Float )
adaptSize dock ( w, h ) =
    case dock of
        TopToBottom -> ( w, h )
        BottomToTop -> ( w, h )
        LeftToRight -> ( h, w )
        RightToLeft -> ( h, w )


firstCellAt : Dock ->  { a | width : Float, height : Float } -> ( Float, Float )
firstCellAt dock bounds =
    case dock of
        TopToBottom -> ( 0, 0 )
        BottomToTop -> ( 0, bounds.height - Cell.height )
        LeftToRight -> ( 0, 0 )
        RightToLeft -> ( bounds.width - Cell.width, 0 )


boundsFromSize
     : Dock
    -> Size Pixels
    -> Size Cells
    -> { x : Float, y : Float, width : Float, height : Float }
boundsFromSize
    dock
    (Size ( viewportWidthInPx, viewportHeightInPx ))
    (Size ( gridWidthInCells, gridHeightInCells )) =
    let
        ( gridWidthInPx, gridHeightInPx ) =
            ( Cell.width * toFloat gridWidthInCells
            , Cell.height * toFloat gridHeightInCells
            )
    in
        { x = case dock of
            TopToBottom -> Cell.gap / 2
            BottomToTop -> Cell.gap / 2
            RightToLeft -> toFloat viewportWidthInPx - gridWidthInPx - Cell.gap / 2
            LeftToRight -> Cell.gap / 2
        , y = case dock of
            TopToBottom -> Cell.gap / 2
            BottomToTop -> toFloat viewportHeightInPx - gridHeightInPx - Cell.gap / 2
            RightToLeft -> Cell.gap / 2
            LeftToRight -> Cell.gap / 2
        , width = gridWidthInPx
        , height = gridHeightInPx
        }
        {-
        { x = (toFloat viewportWidthInPx / 2) - (gridWidthInPx / 2)
        , y = (toFloat viewportHeightInPx / 2) - (gridHeightInPx / 2)
        , width = gridWidthInPx
        , height = gridHeightInPx
        } -}


toString : Dock -> String
toString dock =
    case dock of
        TopToBottom -> "ttb"
        BottomToTop -> "btt"
        LeftToRight -> "ltr"
        RightToLeft -> "rtl"
