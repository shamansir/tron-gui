module Maybe.Extra exposing (..)


import Task as Task


inject : (a, Maybe b) -> Maybe (a, b)
inject ( a, maybeB ) =
    maybeB |> Maybe.map (Tuple.pair a)


toCommand : Maybe msg -> Cmd msg
toCommand maybeMsg =
    case maybeMsg of
        Just msg ->
            Task.succeed msg
                |> Task.perform identity
        Nothing -> Cmd.none


-- FIXME: should be in `List.Extra`
findMap : (a -> Maybe x) -> List a -> Maybe x
findMap toValue =
    List.foldl
        (\item maybeResult ->
            case maybeResult of
                Nothing -> toValue item
                _ -> maybeResult
        )
        Nothing