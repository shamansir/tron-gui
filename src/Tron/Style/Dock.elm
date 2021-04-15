module Tron.Style.Dock exposing
    ( Dock
    , topLeft, topCenter, topRight
    , middleLeft, center, middleRight
    , bottomLeft, bottomCenter, bottomRight
    , adaptBounds, adaptPosition, adaptSize, firstCellAt -- FIXME: do not expose
    , boundsFromSize
    , rootPosition, toDistribution -- FIXME: do not expose
    , toString
    )

{-| # Dock
@docs Dock

# Values
@docs topLeft, topCenter, topRight
@docs middleLeft, center, middleRight
@docs bottomLeft, bottomCenter, bottomRight

# Helpers
@docs adaptBounds, adaptPosition, adaptSize, firstCellAt, boundsFromSize, toString
-}

import Tron.Style.Cell as Cell
import SmartPack as D exposing (Distribution(..))

import Size exposing (..)


type HorzAnchor
    = Left
    | Center
    | Right


type VertAnchor
    = Top
    | Middle
    | Bottom


{-| `Dock` describes the direction in which GUI is oriented and to which corner or side of the page it is "docked".

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


{-| Given current `Dock` setting and size of the screen, move the requsted top-left-based bounds to the new area accoring to the docking state. -}
adaptBounds
     : Dock
    -> SizeF Cells
    -> { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
adaptBounds (Dock ( horz, vert )) (SizeF ( width, height )) innerBounds =
    { width = innerBounds.width
    , height = innerBounds.height
    , x =
        case horz of
            Left -> innerBounds.x
            Center -> innerBounds.x -- (width / 2) - innerBounds.width + innerBounds.x
            Right -> width - innerBounds.x - innerBounds.width
    , y =
        case ( horz, vert ) of
            ( _, Bottom ) -> height - innerBounds.y - innerBounds.height
            _ -> innerBounds.y
    }


{-| Given current `Dock` setting and size of the screen, move the requsted top-left-based point to the new position accoring to the docking state. -}
adaptPosition : Dock -> SizeF Cells -> { x : Float, y : Float } -> { x : Float, y : Float }
adaptPosition (Dock ( horz, vert )) (SizeF ( width, height )) { x, y } =
    { x =
        case ( horz, vert ) of
            ( Center, _ ) -> x
            ( _, Bottom ) -> height - y
            _ -> y
    , y =
        case ( horz, vert ) of
            ( Left, _ ) -> x
            ( Center, Bottom ) -> height - y
            ( Center, _ ) -> y
            ( Right, _ ) -> width - x
    }


{-| Flip the sides if docking happens in the center. -}
adaptSize : Dock -> SizeF Cells -> SizeF Cells
adaptSize (Dock (horz, _)) (SizeF ( w, h )) =
    case horz of
        Center -> SizeF ( w, h )
        _ -> SizeF ( h, w )


{-| Where the first cell is located with current dock setting and Docking state. -}
firstCellAt : Dock ->  { a | width : Float, height : Float } -> ( Float, Float )
firstCellAt (Dock ( horz, vert )) bounds =
    ( case horz of
        Right -> bounds.width - Cell.width
        _ -> 0
    , case vert of
        Bottom -> bounds.height - Cell.height
        _ -> 0
    )


{-| -}
rootPosition : Dock -> Size Cells -> Int -> Int -> ( Int, Int )
rootPosition (Dock ( horz, vert )) (Size (cw, ch)) itemsCount index =
    case ( horz, vert ) of
        ( Left, Top ) ->
            ( index, 0 )
        ( Left, Middle ) ->
            ( 0, ch // 2 - itemsCount // 2 + index )
        ( Left, Bottom ) ->
            ( index, ch - 1 )
        ( Center, Top ) ->
            ( cw // 2 - itemsCount // 2 + index, 0 )
        ( Center, Middle ) ->
            ( cw // 2 - itemsCount // 2 + index, ch // 2 )
        ( Center, Bottom ) ->
            ( cw // 2 - itemsCount // 2 + index, ch - 1 )
        ( Right, Top ) ->
            ( cw - itemsCount + index, 0 )
        ( Right, Middle ) ->
            ( cw - 1, ch // 2 - itemsCount // 2 + index )
        ( Right, Bottom ) ->
            ( cw - itemsCount + index, 0 )


{-| -}
toDistribution : Dock -> Int -> Distribution
toDistribution (Dock (horz, vert)) idx =
    case ( horz, vert ) of
        ( Left, Top ) ->
            D.Down
        ( Left, Middle ) ->
            D.Right
        ( Left, Bottom ) ->
            D.Up
        ( Center, Top ) ->
            D.Down
        ( Center, Middle ) ->
            if idx // 2 == 0
                then D.Up
                else D.Down
        ( Center, Bottom ) ->
            D.Up
        ( Right, Top ) ->
            D.Down
        ( Right, Middle ) ->
            D.Left
        ( Right, Bottom ) ->
            D.Up


{-| -}
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


{-| -}
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
