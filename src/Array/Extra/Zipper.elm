module Array.Extra.Zipper exposing (..)


import Array exposing (Array)
import Array as Array


type Zipper a b
    = Left a
    | Right b
    | Both a b


zip : Array a -> Array b -> Array (Zipper a b)
zip arrayA arrayB =
    if Array.length arrayA >= Array.length arrayB then
        arrayA |>
            Array.indexedMap
                (\index valueA ->
                    case Array.get index arrayB of
                        Just valueB -> Both valueA valueB
                        Nothing -> Left valueA
                )
    else
        arrayB |>
            Array.indexedMap
                (\index valueB ->
                    case Array.get index arrayA of
                        Just valueA -> Both valueA valueB
                        Nothing -> Right valueB
                )


toTuple : Zipper a b -> ( Maybe a, Maybe b )
toTuple zipper =
    case zipper of
        Left a -> ( Just a, Nothing )
        Right b -> ( Nothing, Just b )
        Both a b -> ( Just a, Just b )



map : (a -> b) -> Zipper a a -> Zipper b b
map f = mapAB f f


mapAB : (a -> x) -> (b -> y) -> Zipper a b -> Zipper x y
mapAB fA fB zipper =
    --run (Left >> fA) (Right >> fB) (Both )
    case zipper of
        Left a -> Left <| fA a
        Right b -> Right <| fB b
        Both a b -> Both (fA a) (fB b)


mapA : (a -> x) -> Zipper a b -> Zipper x b
mapA f = mapAB f identity


mapB : (b -> x) -> Zipper a b -> Zipper a x
mapB f = mapAB identity f


fold : (Maybe a -> Maybe b -> c) -> Zipper a b -> c
fold f =
    foldT <| \(maybeA, maybeB) -> f maybeA maybeB


foldT : ((Maybe a, Maybe b) -> c) -> Zipper a b -> c
foldT f = toTuple >> f


run
     : (a -> x)
    -> (b -> x)
    -> (a -> b -> x)
    -> Zipper a b
    -> x
run fA fB fBoth z =
    case z of
        Left a -> fA a
        Right b -> fB b
        Both a b -> fBoth a b