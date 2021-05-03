module Example.Tiler.Util exposing (..)

import Color exposing (Color)


hexToColor : String -> Result String Color
hexToColor str =
    let
        digitToInt digit =
            case digit of
                'A' -> 10
                'B' -> 11
                'C' -> 12
                'D' -> 13
                'E' -> 14
                'F' -> 15
                'a' -> 10
                'b' -> 11
                'c' -> 12
                'd' -> 13
                'e' -> 14
                'f' -> 15
                _ -> digit
                    |> String.fromChar
                    |> String.toInt
                    |> Maybe.withDefault -256
        digitValues =
            str
                |> String.toList
                |> List.drop 1
                |> List.map digitToInt
        values =
            if List.length digitValues == 6 then
                digitValues
                    |> groupByTwo
                    |> List.map
                        (\pair ->
                            case pair of
                                v1::v2::_ -> v1 * 16 + v2
                                _ -> -1
                        )
            else if List.length digitValues == 3 then
                digitValues
                    |> List.map (\v -> v * 16 + v)
            else []

    in
        if (values |> List.filter ((>) 0) |> List.length) > 0 then
            Err str
        else case values of
            r::g::b::_ ->
                Ok <| Color.rgb255 r g b
            _ -> Err str



groupByTwo : List a -> List (List a) -- (a, a)
groupByTwo =
    List.indexedMap Tuple.pair
        >> List.foldr
            (\( index, val ) vals ->
                if remainderBy 2 index == 0 then
                    case vals of
                        [x]::xs ->
                            [ val, x ] :: xs
                        _ -> vals
                else
                    [ val ] :: vals
            )
            []
