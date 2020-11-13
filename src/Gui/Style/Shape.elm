module Gui.Style.Shape exposing
    ( Shape
    , auto, rows, cols
    , by
    , find
    )


import Gui.Style.CellShape exposing (CellShape(..), numify)


type Shape
    = Auto
    | Rows Int
    | Cols Int
    | Shape Int Int


auto : Shape
auto = Auto


rows : Int -> Shape
rows = Rows


cols : Int -> Shape
cols = Cols


by : Int -> Int -> Shape
by = Shape


find : CellShape -> Shape -> List a -> ( Float, Float )
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
