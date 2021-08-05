module Tron.Style.PanelShape exposing
    ( PanelShape, Pagination, Grouping
    , auto, rows, cols
    , by
    , singlePage, manyPages, pagesEnabled, togglePagination
    , distribute, numify, create
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
import Tron.Pages as Pages exposing (Pages)
import Size exposing (..)


maxCols = 3

maxRows = 3


{-| -}
type PanelShape
    = PanelShape Pagination Grouping


{-| -}
type Pagination
    = SinglePage
    | ManyPages


{-| -}
type Grouping
    = Auto
    | Rows Int
    | Cols Int
    | Shape Int Int


{-| Calculate both rows and column numbers automatically, based on the number of cells inside. -}
auto : PanelShape
auto = PanelShape ManyPages Auto


{-| Specify how many cell rows there should be in the panel, and calculate columns number automatically. -}
rows : Int -> PanelShape
rows = PanelShape ManyPages << Rows


{-| Specify how many cell columns there should be in the panel, and calculate rows number automatically. -}
cols : Int -> PanelShape
cols = PanelShape ManyPages << Cols


{-| Specify panel size manually, i.e. how many cells horizontally and how many vertically. -}
by : Int -> Int -> PanelShape
by c r = PanelShape ManyPages <| Shape c r


{-| Do not distribute items over pages -}
singlePage : PanelShape -> PanelShape
singlePage (PanelShape _ ps) = PanelShape SinglePage ps


{-| Distribute items over pages automatically (when number of columns / rows overflows 3). Default condition. -}
manyPages : PanelShape -> PanelShape
manyPages (PanelShape _ ps) = PanelShape ManyPages ps


{-| Check if pagination is enabled. -}
pagesEnabled : PanelShape -> Bool
pagesEnabled (PanelShape pages _) =
    case pages of
        SinglePage -> False
        ManyPages -> True


{-| Turn pagination off or on. -}
togglePagination : PanelShape -> PanelShape
togglePagination (PanelShape pages ps) =
    case pages of
        SinglePage -> PanelShape ManyPages ps
        ManyPages -> PanelShape SinglePage ps


{-| Get numeric size of a panel in cells, and a set of pages required, if there are overflows. Floats, since there could be half-cells. -}
distribute : PanelShape -> CellShape -> List a -> ( Pages (List a), SizeF Cells )
distribute (PanelShape pagination grouping) cellShape items =
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
            )


{-| Returns columns and rows to take, and -1 is the value should be auto-calculated.
-}
numify : PanelShape -> ( Int, Int )
numify (PanelShape _ ps) =
    case ps of
        Auto -> ( -1, -1 )
        Rows n -> ( -1, n )
        Cols n -> ( n, -1 )
        Shape nc nr -> ( nc, nr )


{-| Create panel shape from its numeric representation. Put -1 for auto calculation.
-}
create : ( Int, Int ) -> PanelShape
create ( nc, nr ) =
    PanelShape ManyPages
         <| if (nc == -1) && (nr == -1) then
                Auto
            else if (nc == -1) then
                Rows nr
            else if (nr == -1) then
                Cols nc
            else
                Shape nc nr
