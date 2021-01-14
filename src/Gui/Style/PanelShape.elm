module Gui.Style.PanelShape exposing
    ( PanelShape
    , auto, rows, cols
    , by
    , find
    )


{-| # Panel Shape

Panel shape is how many cells the panel takes in the GUI grid (it is automatically calculated considering the fact that cells inside could be halflings or giants, see `CellShape`).

You are not required to specify both sides, just use `rows` or `cols` helpers to say how many rows or columns you want in the panel and other side will be calculated automatically. Or even use `auto` and both sides will be suggested, but this usually doesn't look good. To specify both sides manually, use `by`.

@docs PanelShape

# Automatic
@docs auto, rows, cols

# Manual
@docs by

# Helpers
@docs find
-}


import Gui.Style.CellShape exposing (CellShape(..), numify)


{-| -}
type PanelShape
    = Auto
    | Rows Int
    | Cols Int
    | Shape Int Int


{-| Calculate both rows and column numbers automatically, based on the number of cells inside. -}
auto : PanelShape
auto = Auto


{-| Specify how many cell rows there should be in the panel, and calculate columns number automatically. -}
rows : Int -> PanelShape
rows = Rows


{-| Specify how many cell columns there should be in the panel, and calculate rows number automatically. -}
cols : Int -> PanelShape
cols = Cols


{-| Specify panel size manually, i.e. how many cells horizontally and how many vertically. -}
by : Int -> Int -> PanelShape
by = Shape


{-| Get numeric size of a panel in cells. Floats, since there could be half-cells. -}
find : CellShape -> PanelShape -> List a -> ( Float, Float )
find cellShape shape items =
    let
        ( cellXMultiplier, cellYMultiplier ) =
            numify cellShape
        otherSide n =
            if (modBy n <| List.length items) == 0
                then List.length items // n
                else List.length items // n + 1
    in
        ( case shape of
            Auto ->
                let n = List.length items // 2
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
        ) |> Tuple.mapBoth
            (\c -> toFloat c * cellXMultiplier)
            (\r -> toFloat r * cellYMultiplier)
