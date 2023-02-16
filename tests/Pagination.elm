module Pagination exposing (..)


import Tron.Pages as Pages


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


alphabet : List String
alphabet = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]


suite : Test
suite =
    describe "pagination"

        [ describe "distribute"
            [ test "distribution by 1"
                <| \_ ->
                    Expect.equal
                        (Pages.distribute (Pages.Maximum 1) [ "A", "B", "C" ]
                            |> Pages.toList
                        )
                        [ ["A"], ["B"], ["C"] ]

            , test "distribution by 3"
                <| \_ ->
                    Expect.equal
                        (Pages.distribute (Pages.Maximum 3) [ "A", "B", "C" ]
                            |> Pages.toList
                        )
                        [ [ "A", "B", "C" ] ]

            , test "distribution by 3, revisitied"
                <| \_ ->
                    Expect.equal
                        (Pages.distribute (Pages.Maximum 3) alphabet
                            |> Pages.toList
                        )
                        [ [ "A", "B", "C" ]
                        , [ "D", "E", "F" ]
                        , [ "G", "H", "I" ]
                        , [ "J", "K", "L" ]
                        , [ "M", "N", "O" ]
                        , [ "P", "Q", "R" ]
                        , [ "S", "T", "U" ]
                        , [ "V", "W", "X" ]
                        , [ "Y", "Z" ]
                        ]

            , test "distribution by 2"
                <| \_ ->
                    Expect.equal
                        (Pages.distribute (Pages.Maximum 2) [ "A", "B", "C", "D", "E" ]
                            |> Pages.toList
                        )
                        [ [ "A", "B" ], [ "C", "D" ], [ "E" ] ]
            ]

        , describe "distribute over"
            [ test "distribute over zero pages"
                <| \_ ->
                    Expect.equal
                        (Pages.distributeOver (Pages.Count 0) alphabet
                            |> Tuple.second
                            |> Pages.toList
                        )
                        [ [ ] ]
            , test "distribute over one page"
                <| \_ ->
                    Expect.equal
                        (Pages.distributeOver (Pages.Count 1) alphabet
                            |> Tuple.second
                            |> Pages.toList
                        )
                        [ [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ] ]
            , test "distribute over 4 pages"
                <| \_ ->
                    Expect.equal
                        (Pages.distributeOver (Pages.Count 4) alphabet
                            |> Tuple.second
                            |> Pages.toList
                        )
                        [ [ "A", "B", "C", "D", "E", "F", "G", "H" ]
                        , [ "I", "J", "K", "L", "M", "N", "O", "P" ]
                        , [ "Q", "R", "S", "T", "U", "V", "W", "X" ]
                        , [ "Y", "Z" ]
                        ]
            , test "distribute over 5 pages"
                <| \_ ->
                    Expect.equal
                        (Pages.distributeOver (Pages.Count 5) alphabet
                            |> Tuple.second
                            |> Pages.toList
                        )
                        [ [ "A", "B", "C", "D", "E", "F" ]
                        , [ "G", "H", "I", "J", "K", "L" ]
                        , [ "M", "N", "O", "P", "Q", "R" ]
                        , [ "S", "T", "U", "V", "W", "X" ]
                        , [ "Y", "Z" ]
                        ]
            ]
        ]
