module Array.Extra exposing (..)


import Array exposing (Array)


findMap : (a -> Maybe x) -> Array a -> Maybe x
findMap toValue =
    Array.foldl
        (\item maybeResult ->
            case maybeResult of
                Nothing -> toValue item
                _ -> maybeResult
        )
        Nothing


filterMap : (a -> Maybe b) -> Array a -> Array b
filterMap f =
    Array.toList >> List.filterMap f >> Array.fromList
    -- FIXME: faster with `fold`


catMaybes : Array (Maybe a) -> Array a
catMaybes =
    filterMap identity


{-
zip3Arrays : Array a -> Array b -> Array c -> Array ( Maybe a, Maybe b, Maybe c )
zip3Arrays arrayA arrayB arrayC =
    if Array.length arrayC < Array.length arrayA || Array.length arrayC < Array.length arrayB then
        zipArrays arrayA arrayB
            |> Array.indexedMap
                (\index ( maybeValueA, maybeValueB ) ->
                    ( maybeValueA, maybeValueB, Array.get index arrayC )
                )
    else if Array.length arrayA >= Array.length arrayB then
        zipArrays arrayA arrayC
            |> Array.indexedMap
                (\index ( maybeValueA, maybeValueC ) ->
                    ( maybeValueA, Array.get index arrayB, maybeValueC )
                )
    else
        zipArrays arrayB arrayC
            |> Array.indexedMap
                (\index ( maybeValueB, maybeValueC ) ->
                    ( Array.get index arrayA, maybeValueB, maybeValueC )
                )
-}