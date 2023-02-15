module Tron.Style.PanelShape exposing
    ( PanelShape, Pagination, Paving
    , auto, rows, cols
    , by
    , singlePage, exact, pagesEnabled
    , distribute
    , numify, create
    )


{-| # Panel Shape

Panel shape is how many cells the panel takes in the GUI grid (it is automatically calculated considering the fact that cells inside could be halflings or giants, see `CellShape`).

You are not required to specify both sides, just use `rows` or `cols` helpers to say how many rows or columns you want in the panel and other side will be calculated automatically. Or even use `auto` and both sides will be suggested, but this usually doesn't look good. To specify both sides manually, use `by`.

@docs PanelShape, Pagination, Grouping

# Automatic
@docs auto, rows, cols

# Manual
@docs by

# Pagination
@docs distribute, singlePage, manyPages, pagesEnabled, togglePagination

# Helpers
@docs numify, create
-}


import Tron.Style.CellShape as CS exposing (CellShape(..))
import Tron.Pages as Pages exposing (Pages, PageRef)
import Size exposing (..)


defaultMaxCols = 3

defaultMaxRows = 3


{-| -}
type PanelShape
    = PanelShape Pagination


{-| -}
type Pagination
    = SinglePage -- FIXME : == ExactPages 1
    -- Scroll
    | Distribute Paving
    | ExactPages Int


{-| -}
type Paving
    = Auto -- { maxInColumn = 3, maxInRow = 3 }
    | Exact { maxInColumn : Int, maxInRow : Int }
    | ByRows { maxInColumn : Int } -- , TODO: pages : Int
    | ByCols { maxInRow : Int } -- , TODO: pages : Int


{-| Calculate both rows and column numbers automatically, based on the number of cells inside. -}
auto : PanelShape
auto = PanelShape <| Distribute Auto


{-| Specify how many cell rows there should be in the panel, and calculate columns number automatically. -}
rows : Int -> PanelShape
rows n = PanelShape <| Distribute <| ByRows { maxInColumn = n }


{-| Specify how many cell columns there should be in the panel, and calculate rows number automatically. -}
cols : Int -> PanelShape
cols n = PanelShape <| Distribute <| ByCols { maxInRow = n }


{-| Specify panel size manually, i.e. how many cells horizontally and how many vertically. -}
by : Int -> Int -> PanelShape
by c r = PanelShape <| Distribute <| Exact { maxInColumn = c, maxInRow = r }


{-| Do not distribute items over pages -}
singlePage : PanelShape
singlePage = PanelShape SinglePage


{-| Distribute items over an exact amount of pages -}
exact : Int -> PanelShape
exact = PanelShape << ExactPages


{-| Check if pagination is enabled. -}
pagesEnabled : PanelShape -> Bool
pagesEnabled (PanelShape pages) =
    case pages of
        SinglePage -> False
        Distribute _ -> True
        ExactPages _ -> True


{-| Get numeric size of a panel in cells, and a set of pages required, if there are overflows. Floats, since there could be half-cells. -}
distribute : PanelShape -> CellShape -> PageRef -> List a ->  ( Pages (List a), SizeF Cells )
distribute (PanelShape paging) cellShape ref items =
    -- FIXME : use pageRef
    let
        itemCount = List.length items
        ( cellXMultiplier, cellYMultiplier ) =
            CS.numify cellShape
        otherSide n =
            if (n /= 0) && (modBy n itemCount) == 0
                then itemCount // n
                else itemCount // n + 1
        default = ( Pages.single items, SizeF (0, 0) )
        onAPage ( c, r ) =
            ceiling
                ( toFloat c * cellXMultiplier
                * toFloat r * cellYMultiplier
                )
        pagesFor =
            Pages.distribute << Pages.Maximum << onAPage
    in
        case paging of
            SinglePage ->
                let oneSide = ceiling (toFloat itemCount / 2)
                in
                    ( Pages.single items
                    , SizeF
                        ( toFloat oneSide * cellXMultiplier
                        , toFloat (otherSide oneSide) * cellYMultiplier
                        )
                    )

            Distribute paving ->
                case paving of
                    Auto ->
                        ( pagesFor ( defaultMaxCols, defaultMaxRows ) items
                        , SizeF ( defaultMaxCols, defaultMaxRows )
                        )
                    Exact { maxInColumn, maxInRow } ->
                        ( pagesFor ( maxInColumn, maxInRow ) items
                        , SizeF ( toFloat maxInColumn, toFloat maxInRow )
                        )
                    ByCols { maxInRow } ->
                        ( pagesFor ( otherSide maxInRow, maxInRow ) items
                        , SizeF ( toFloat <| otherSide maxInRow, toFloat maxInRow )
                        )
                    ByRows { maxInColumn } ->
                        ( pagesFor ( maxInColumn, otherSide maxInColumn ) items
                        , SizeF ( toFloat maxInColumn, toFloat <| otherSide maxInColumn )
                        )

            ExactPages n ->
                let
                    ( maxOnAPage, pages )
                        = Pages.distributeOver (Pages.Count n) items
                    oneSide = ceiling (toFloat maxOnAPage / 2)
                in
                    ( pages
                    , SizeF
                        ( toFloat oneSide * cellXMultiplier
                        , toFloat (otherSide oneSide) * cellYMultiplier
                        )
                    )

    {-}
    let
        itemCount = List.length items
        ( cellXMultiplier, cellYMultiplier ) =
            CS.numify cellShape
        otherSide n =
            if (n /= 0) && (modBy n itemCount) == 0
                then itemCount // n
                else itemCount // n + 1
        onAPage ( c, r ) =
            ceiling
                ( toFloat c * cellXMultiplier
                * toFloat r * cellYMultiplier
                )
        pagesFor =
            Pages.distribute << onAPage

    in
        ( case grouping of
            Auto ->
                let n = itemCount // 2
                in
                    ( n
                    , otherSide n
                    )
            Cols n ->
                ( n
                , otherSide n
                )
            Rows n ->
                ( otherSide n
                , n
                )
            Shape nc nr ->
                ( nc
                , nr
                )
        )
        |>
            (\(c, r) ->
                case pagination of
                    ManyPages ->
                        if c > maxCols
                            then
                                ( pagesFor ( maxCols, r ) items
                                , ( maxCols, r )
                                )
                            else if r > maxRows
                            then
                                ( pagesFor ( c, maxRows ) items
                                , ( c, maxRows )
                                )
                            else
                                ( Pages.single <| items
                                , (c, r)
                                )
                    SinglePage ->
                        ( Pages.single <| items
                        , (c, r)
                        )
            )
        |> Tuple.mapSecond
            (Tuple.mapBoth
                (\c -> toFloat c * cellXMultiplier)
                (\r -> toFloat r * cellYMultiplier)
                >> SizeF
            ) -}


type alias QuickDef = { pages : Int, cols : Int, rows : Int }


{-| Returns columns and rows to take, and -1 is the value should be auto-calculated.
-}
numify : PanelShape -> QuickDef
numify (PanelShape ps) =
    case ps of
        SinglePage -> { pages = 1, cols = -1, rows = -1 }
        ExactPages n -> { pages = n, cols = -1, rows = -1 }
        Distribute paving ->
            case paving of
                Auto -> { pages = -1, cols = -1, rows = -1 }
                Exact { maxInColumn, maxInRow } -> { pages = -1, cols = maxInColumn, rows = maxInRow }
                ByRows { maxInColumn } -> { pages = -1, cols = maxInColumn, rows = -1 }
                ByCols { maxInRow } -> { pages = -1, cols = -1, rows = maxInRow }


{-| Create panel shape from its numeric representation. Put -1 for auto calculation.
-}
create : { pages : Int, cols : Int, rows : Int } -> PanelShape
create ps =
    PanelShape <|
        if ps.pages == 1 then SinglePage
        else if ps.pages >= 0 then ExactPages ps.pages
        else if (ps.cols == -1) && (ps.rows == -1) then Distribute Auto
        else if (ps.cols == -1) then Distribute <| ByCols { maxInRow = ps.rows }
        else if (ps.rows == -1) then Distribute <| ByRows { maxInColumn = ps.cols }
        else Distribute <| Exact { maxInColumn = ps.cols, maxInRow = ps.rows }