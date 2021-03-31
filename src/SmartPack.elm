module SmartPack exposing (..)


import Array exposing (Array)


import BinPack exposing (Bounds)
import Size exposing (..)


type SmartPack a = SmartPack (Size Cells) (List (Bounds, a))


toMatrix : SmartPack a -> Array (Array ( Int, Maybe a ))
toMatrix (SmartPack (Size (w, h)) items) =
    let
        addBounds bounds matrix =
            matrix
    in items
        |> List.sortBy (Tuple.first >> .x)
        |> List.foldl
            (\(bounds, v) ->
                addBounds bounds
            )
            (Array.repeat w (Array.repeat h ( 0, Nothing )))


container : Size Cells -> SmartPack a
container size = SmartPack size []


pack : SizeF Cells -> a -> SmartPack a -> Maybe (SmartPack a)
pack _ _ s = Just s


packAt : ( Float, Float ) -> SizeF Cells -> a -> SmartPack a -> Maybe (SmartPack a)
packAt _ _ _ s = Just s


carelessPack : SizeF Cells -> a -> SmartPack a -> SmartPack a
carelessPack _ _ s = s


carelessPackAt : ( Float, Float ) -> SizeF Cells -> a -> SmartPack a -> SmartPack a
carelessPackAt _ _ _ s = s


asList : SmartPack a -> List (Bounds, a)
asList (SmartPack _ items) = items


dimensions : SmartPack a -> Size Cells
dimensions (SmartPack size _) = size
