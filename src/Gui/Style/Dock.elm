module Gui.Style.Dock exposing
    ( Dock
    , topLeft, topCenter, topRight
    , middleLeft, center, middleRight
    , bottomLeft, bottomCenter, bottomRight
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
middleLeft = Dock ( Left, Middle )


{-|
-}
center : Dock
center = Dock ( Center, Middle )


{-|
-}
middleRight : Dock
middleRight = Dock ( Right, Middle )


{-|
-}
bottomLeft : Dock
bottomLeft = Dock ( Left, Bottom )


{-|
-}
bottomCenter : Dock
bottomCenter = Dock ( Center, Bottom )


{-|
-}
bottomRight : Dock
bottomRight = Dock ( Right, Bottom )


adaptBounds
     : Dock
    -> ( Float, Float )
    -> { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
adaptBounds (Dock ( horz, vert )) ( width, height ) innerBounds =
    { width =
        case horz of
            Left -> innerBounds.height
            Center -> innerBounds.width
            Right -> innerBounds.height
    , height =
        case horz of
            Left -> innerBounds.width
            Center -> innerBounds.height
            Right -> innerBounds.width
    , x =
        case horz of
            Left -> innerBounds.y
            Center -> innerBounds.x
            Right -> height - innerBounds.y - innerBounds.height
    , y =
        case ( horz, vert ) of
            ( Center, Bottom ) -> height - innerBounds.y - innerBounds.height
            ( Center, _ ) -> innerBounds.y
            ( _, Bottom ) -> width - innerBounds.x - innerBounds.width
            _ -> innerBounds.x
    }


adaptPosition : Dock -> ( Float, Float ) -> { x : Float, y : Float } -> { x : Float, y : Float }
adaptPosition (Dock ( horz, vert )) ( width, height ) { x, y } =
    { x =
        case ( horz, vert ) of
            ( Left, Top ) -> y
            ( Left, Middle ) -> y
            ( Left, Bottom ) -> height - y
            ( Center, Top ) -> x
            ( Center, Middle ) -> x
            ( Center, Bottom ) -> x
            ( Right, Top ) -> y
            ( Right, Middle ) -> y
            ( Right, Bottom ) -> height - y
    , y =
        case Debug.log "dock" ( horz, vert ) of
            ( Left, Top ) -> x
            ( Left, Middle ) -> x
            ( Left, Bottom ) -> x
            ( Center, Top ) -> y
            ( Center, Middle ) -> y
            ( Center, Bottom ) -> height - y
            ( Right, Top ) -> width - x
            ( Right, Middle ) -> width - x
            ( Right, Bottom ) -> width - x

            -- ( Center, Bottom ) -> height - y
            -- ( Center, _ ) -> y
            -- ( _, Bottom ) -> width - x
            -- _ -> x
    } |> Debug.log "pos"


adaptSize : Dock -> ( Float, Float ) -> ( Float, Float )
adaptSize (Dock (horz, _)) ( w, h ) =
    case horz of
        Center -> ( w, h )
        _ -> ( h, w )


firstCellAt : Dock ->  { a | width : Float, height : Float } -> ( Float, Float )
firstCellAt (Dock ( horz, vert )) bounds =
    ( case horz of
        Right -> bounds.height - Cell.height
        _ -> 0
    , case ( horz, vert ) of
       ( Left, Bottom ) -> bounds.width - Cell.width
       ( Center, Bottom ) -> bounds.height - Cell.height
       ( Right, Bottom ) -> bounds.width - Cell.width
       _ -> 0
    )


boundsFromSize
     : Dock
    -> Size Pixels
    -> Size Cells
    -> { x : Float, y : Float, width : Float, height : Float }
boundsFromSize
    (Dock ( horz, vert ))
    (Size ( viewportWidthInPx, viewportHeightInPx ))
    (Size ( gridWidthInCells, gridHeightInCells )) =
    let
        ( gridWidthInPx, gridHeightInPx ) =
            ( Cell.width * toFloat gridWidthInCells
            , Cell.height * toFloat gridHeightInCells
            )
    in
        { x = case horz of
            Left -> Cell.gap / 2
            Center -> (toFloat viewportWidthInPx / 2) - (gridWidthInPx / 2)
            Right -> toFloat viewportWidthInPx - gridWidthInPx - Cell.gap / 2
        , y = case vert of
            Top -> Cell.gap / 2
            Middle -> (toFloat viewportHeightInPx / 2) - (gridHeightInPx / 2)
            Bottom -> toFloat viewportHeightInPx - gridHeightInPx - Cell.gap / 2
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
toString (Dock ( horz, vert )) =
    ( case horz of
        Left -> "left"
        Center -> "center"
        Right -> "right"
    ) ++ "-" ++
    ( case vert of
        Top -> "top"
        Middle -> "middle"
        Bottom -> "bottom"
    )
