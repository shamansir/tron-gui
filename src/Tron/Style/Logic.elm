module Tron.Style.Logic exposing (..)


import Dict exposing (Dict)
import Bounds exposing (..)

import Color exposing (Color)
import Size exposing (..)
import SmartPack as D exposing (Distribution)

import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CellShape exposing (toString)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Cell as Cell
import Tron.Style.Dock as Dock exposing (Dock, HorzAnchor(..), VertAnchor(..))


import Tron.Mouse exposing (Position)
import Tron.Property as Property exposing (Property(..))
import Tron.Path as Path exposing (Path, toString)
import Tron.Focus exposing (Focused(..))



-- white = Color.white


-- label = Color.rgb255 144 144 144 -- "#909090"



-- canvasBackground = Color.lightGray


-- transparent = Color.rgba 0.0 0.0 0.0 0.0



-- TODO: make bounds to be bounded to pariticular units
toGridCoords : BoundsF -> Position -> Position
toGridCoords bounds pos =
    { x = (pos.x - bounds.x) / Cell.width
    , y = (pos.y - bounds.y) / Cell.height
    }


shapeToModifier : CellShape -> String
shapeToModifier =
    CellShape.toString


{-| Given current `Dock` setting and size of the screen, move the requsted top-left-based bounds to the new area accoring to the docking state. -}
adaptBounds
     : Dock
    -> SizeF Cells
    -> { x : Float, y : Float, width : Float, height : Float }
    -> { x : Float, y : Float, width : Float, height : Float }
adaptBounds dock (SizeF ( width, height )) innerBounds =
    { width = innerBounds.width
    , height = innerBounds.height
    , x =
        case Dock.horzAnchor dock of
            Left -> innerBounds.x
            Center -> innerBounds.x -- (width / 2) - innerBounds.width + innerBounds.x
            Right -> width - innerBounds.x - innerBounds.width
    , y =
        case Dock.anchors dock of
            ( _, Bottom ) -> height - innerBounds.y - innerBounds.height
            _ -> innerBounds.y
    }


{-| Given current `Dock` setting and size of the screen, move the requsted top-left-based point to the new position accoring to the docking state. -}
adaptPosition : Dock -> SizeF Cells -> { x : Float, y : Float } -> { x : Float, y : Float }
adaptPosition dock (SizeF ( width, height )) { x, y } =
    { x =
        case Dock.anchors dock of
            ( Center, _ ) -> x
            ( _, Bottom ) -> height - y
            _ -> y
    , y =
        case Dock.anchors dock of
            ( Left, _ ) -> x
            ( Center, Bottom ) -> height - y
            ( Center, _ ) -> y
            ( Right, _ ) -> width - x
    }


{-| Flip the sides if docking happens in the center. -}
adaptSize : Dock -> SizeF Cells -> SizeF Cells
adaptSize dock (SizeF ( w, h )) =
    case Dock.horzAnchor dock of
        Center -> SizeF ( w, h )
        _ -> SizeF ( h, w )


{-| Where the first cell is located with current dock setting and Docking state. -}
firstCellAt : Dock ->  Size Cells -> Int -> ( Int, Int )
firstCellAt dock size itemsCount =
    rootPosition dock size itemsCount 0


{-| -}
rootPosition : Dock -> Size Cells -> Int -> Int -> ( Int, Int )
rootPosition dock (Size (cw, ch)) itemsCount index =
    case Dock.anchors dock of
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
toDistribution dock idx =
    case Dock.anchors dock of
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
    dock
    (Size ( viewportWidthInPx, viewportHeightInPx ))
    (Size ( gridWidthInCells, gridHeightInCells )) =
    let
        ( gridWidthInPx, gridHeightInPx ) =
            ( Cell.width * Basics.toFloat gridWidthInCells
            , Cell.height * Basics.toFloat gridHeightInCells
            )
    in
        { x = case Dock.horzAnchor dock of
            Left -> Cell.gap / 2
            Center -> (Basics.toFloat viewportWidthInPx / 2) - (gridWidthInPx / 2)
            Right -> Basics.toFloat viewportWidthInPx - gridWidthInPx - Cell.gap / 2
        , y = case Dock.vertAnchor dock of
            Top -> Cell.gap / 2
            Middle -> (Basics.toFloat viewportHeightInPx / 2) - (gridHeightInPx / 2)
            Bottom -> Basics.toFloat viewportHeightInPx - gridHeightInPx - Cell.gap / 2
        , width = gridWidthInPx
        , height = gridHeightInPx
        }
        {-
        { x = (toFloat viewportWidthInPx / 2) - (gridWidthInPx / 2)
        , y = (toFloat viewportHeightInPx / 2) - (gridHeightInPx / 2)
        , width = gridWidthInPx
        , height = gridHeightInPx
        } -}



paginationMaskIdFor : Path -> String
paginationMaskIdFor path = "pagination-mask-" ++ Path.toString path