module Pagination exposing (..)


import Tron.Pages as Pages


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "pagination"
        [ test "distribution by 1"
            <| \_ ->
                Expect.equal
                    (Pages.distribute 1 [ "A", "B", "C" ]
                        |> Pages.toList
                    )
                    [ ["A"], ["B"], ["C"] ]

        , test "distribution by 3"
            <| \_ ->
                Expect.equal
                    (Pages.distribute 3 [ "A", "B", "C" ]
                        |> Pages.toList
                    )
                    [ [ "A", "B", "C" ] ]

        , test "distribution by 3, revisitied"
            <| \_ ->
                Expect.equal
                    (Pages.distribute 3 [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]
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
        ]
