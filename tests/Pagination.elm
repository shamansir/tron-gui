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
                    (Pages.distribute 1 [ "A", "B", "C" ] |> Pages.toList)
                    [ ["A"], ["B"], ["C"] ]
        ]
