module Gui.Render.Shape exposing
    ( Shape
    , auto, rows, cols
    , by
    , find
    )


import Gui.Render.Style exposing (CellShape)


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
find _ _ _ = ( 0, 0 )


{-
if (modBy 2 <| List.length options) == 0
            then List.length options // 2
            else List.length options // 2 + 1
-}
