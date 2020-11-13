module Gui.Render.Shape exposing
    ( Shape
    , auto, rows, cols
    , by
    , find
    )


import Gui.Render.Style exposing (CellShape(..))


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
            cellSizeFromShape cellShape
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


cellSizeFromShape : CellShape -> ( Float, Float )
cellSizeFromShape cs =
    case cs of
       Full -> ( 1.0, 1.0 )
       Half -> ( 0.5, 0.5 )
       HalfByOne -> ( 0.5, 1.0 )
       OneByHalf -> ( 1.0, 0.5 )
       TwiceByHalf -> ( 2.0, 0.5 )
       HalfByTwice -> ( 0.5, 2.0 )
       TwiceByTwice -> ( 2.0, 2.0 )
