module Gui.Grid exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Control exposing (..)
import Gui.Nest exposing (..)


type alias GridCell umsg =
    { cell: Cell umsg
    , nestPos: NestPos
    -- if it's Choice Item, then we'll call this handler on click:
    , onSelect : Maybe (String -> Maybe umsg)
     -- if it's Choice item, then it has selection state:
    , isSelected: Maybe SelectionState
    , isFocused: FocusState
    }


type GridPos = GridPos Int Int
type alias Row umsg = Array (Maybe (GridCell umsg))
type alias Rows umsg = Array (Row umsg)
type Grid umsg = Grid Shape (Rows umsg)


type Mode
    = DebugInfo
    | Fancy


type FocusState
    = Focused Int -- nest level
    | NotFocused


mode : Mode
mode = Fancy


emptyGrid : Shape -> Grid umsg
emptyGrid (( width, height ) as shape)
    = Grid shape <| Array.repeat height (Array.repeat width Nothing)


bottomLeft : GridPos
bottomLeft = (GridPos 0 0)


putAtRoot : GridPos -> Nest umsg -> Grid umsg -> Grid umsg
putAtRoot gridPos nest grid =
    put gridPos Nothing Nothing Nothing nest grid


put
    :  GridPos
    -> Maybe ItemChosen
    -> Maybe NestPos
    -> Maybe (ChoiceHandler umsg)
    -> Nest umsg
    -> Grid umsg
    -> Grid umsg
put
    (GridPos row col)
    maybeChosenItem
    maybeParent
    maybeSelectHandler
    nest
    (Grid gridShape rows) =
    let
        ( gridWidth, _ ) = gridShape
        parentNestLevel = maybeParent
            |> Maybe.map getNestLevel
            |> Maybe.withDefault 0
        currentShape = nest.shape
        cellsList = nest.cells
        cells = Array.fromList cellsList
            |> Array.indexedMap
                (\cellIndex cell ->
                    let nestPos = maybeParent |> deeperOrRoot cellIndex
                    in  { cell = cell
                        , nestPos = nestPos
                        , onSelect = maybeSelectHandler -- we assume it is the choice item, if Just
                            |> Maybe.map ((|>) cellIndex)
                        , isSelected = case maybeChosenItem of
                            Just chosenIndex ->
                                Just <|
                                    if cellIndex == chosenIndex
                                    then Selected else NotSelected
                            _ -> Nothing
                        , isFocused = if nest.focus == cellIndex
                            then Focused <| getNestLevel nestPos
                            else NotFocused
                        }
                )
        fits ( locRow, locCol ) ( width, height ) =
            (locRow < height) && ( locCol < width )
        indexOf ( locRow, locCol ) ( width, _ ) =
            locRow * width + locCol
        updateCell locRow locCol prevCell =
            if (locRow >= row) && (locCol >= col) then
                let
                    localPos = (locRow - row, locCol - col)
                in
                    if fits localPos currentShape then
                        case Array.get (indexOf localPos currentShape) cells of
                            Just newCell -> Just newCell
                            Nothing -> prevCell
                    else prevCell
            else prevCell
        updateRow rowIndex innerRow =
            innerRow |> Array.indexedMap (updateCell rowIndex)
        findNextPos row_ col_ ( curWidth, curHeight ) ( nestedWidth, nestedHeight ) =
            if (col_ + nestedWidth < gridWidth) then
                GridPos (row_ + curHeight) col_
            else GridPos (row_ + curHeight) (gridWidth - nestedWidth)
        applyColExpands maybeCell ( locCol, grid ) =
            ( locCol + 1
            , case maybeCell of
                Just { cell, nestPos } ->
                    let
                        ( cellNestLevel, cellIndex ) =
                            ( getNestLevel nestPos
                            , getIndexOf nestPos |> Maybe.withDefault -1
                            )
                    in if (cellNestLevel == parentNestLevel + 1) then
                        case cell of
                            Nested _ Expanded ({ shape } as innerNest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    Nothing
                                    (Just nestPos)
                                    Nothing
                                    innerNest
                                    grid
                            Choice _ Expanded selectedItem handler ({ shape } as innerNest) ->
                                put
                                    (findNextPos row col currentShape shape)
                                    (Just selectedItem)
                                    (Just nestPos)
                                    (Just handler)
                                    innerNest
                                    grid
                            _ -> grid
                    else grid
                _ -> grid
            )
        applyExpands locRow grid =
            Array.foldl applyColExpands ( 0, grid ) locRow
                |> Tuple.second
    in
        rows
            |> Array.indexedMap updateRow
            |> (\innerRows ->
                    Array.foldl applyExpands (Grid gridShape innerRows) innerRows
                )


set : GridPos -> GridCell umsg -> Grid umsg -> Grid umsg
set (GridPos row col) cell ((Grid shape rows) as grid) =
    Array.get row rows
        |> Maybe.map
            (\prevRow ->
                Array.set col (Just cell) prevRow
            )
        |> Maybe.map
            (\newRow ->
                Array.set row newRow rows)
        |> Maybe.map (Grid shape)
        |> Maybe.withDefault grid


layout : Nest umsg -> Grid umsg
layout nest =
    emptyGrid (sizeOf nest, 6)
        |> putAtRoot (GridPos 0 0) nest
        |> flip


flip : Grid umsg -> Grid umsg
flip (Grid shape rows) =
    rows
        |> Array.toList
        |> List.reverse
        |> Array.fromList
        |> Grid shape


fold : (GridPos -> GridCell umsg -> a -> a) -> a -> Grid umsg -> a
fold f init (Grid _ rows) =
    rows
        |> Array.indexedMap Tuple.pair
        |> Array.foldl
            (\(rowIndex, row) lastAtRow ->
                row
                    |> Array.indexedMap Tuple.pair
                    |> Array.foldl
                        (\(cellIndex, maybeCell) lastAtCell ->
                            case maybeCell of
                                Just cell ->
                                    f (GridPos rowIndex cellIndex) cell lastAtCell
                                Nothing -> lastAtCell
                        )
                        lastAtRow
            )
            init


findGridCell : NestPos -> Grid umsg -> Maybe (GridCell umsg)
findGridCell searchFor (Grid _ rows) =
    rows |> Array.foldl
        (\row foundCell ->
            row |> Array.foldl
                (\maybeGridCell cellFoundInRow ->
                    case ( cellFoundInRow, maybeGridCell ) of
                        ( Nothing, Just ({ nestPos } as gridCell) ) ->
                            if (isSamePos searchFor nestPos) then
                                Just gridCell
                            else Nothing
                        _ -> cellFoundInRow
                ) foundCell
        ) Nothing


findCellAt : { x : Int, y : Int } -> Grid umsg -> Maybe (GridCell umsg)
findCellAt { x, y } grid =
    grid
        |> fold (\(GridPos row col) gridCell maybeFound ->
            case maybeFound of
                Just foundCell -> Just foundCell
                Nothing ->
                    if x > ((col - 1) * (cellWidth  + cellMargin * 2))
                    && y > ((row - 1) * (cellHeight + cellMargin * 2))
                    && x < ((col + 1) * (cellWidth  + cellMargin * 2))
                    && y < ((row + 1) * (cellHeight + cellMargin * 2))
                        then Just gridCell
                        else Nothing
        ) Nothing


getShape : Grid umsg -> Shape
getShape (Grid shape _) = shape


getSizeInPixels : Grid umsg -> ( Int, Int )
getSizeInPixels grid =
    case getShape grid of
        ( widthInCells, heightInCells ) ->
            (
                (widthInCells * cellWidth) +
                ((widthInCells - 1) * cellMargin * 2)
            ,
                (heightInCells * cellHeight) +
                ((heightInCells - 1) * cellMargin * 2)
            )
