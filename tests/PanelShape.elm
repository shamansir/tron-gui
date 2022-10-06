module PanelShape exposing (..)


import Json.Encode as E

import Tron exposing (Tron)
import Tron.Core as Core
import Tron.Tree as T exposing (..)
import Tron.Tree.Build.Unit as B


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tron.Pages as P
import Tron.Style.PanelShape as PS
import Tron.Style.CellShape as CS


alphabet : List String
alphabet = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]


aToJ : List String
aToJ = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" ]


suite : Test
suite =
    describe "panel shape"
        [ pagination
        , todo "selecting page"
        ]


pagination : Test
pagination =
    describe "pagination"
        [ describe "distribute `by`"
            [ test "distribute 1x1"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.by 1 1) CS.single P.first [ "A", "B", "C" ]
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A"], ["B"], ["C"] ]
            , test "distribute 2x2"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.by 2 2) CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ [ "A", "B", "C", "D" ]
                        , [ "E", "F", "G", "H" ]
                        , [ "I", "J" ]
                        ]
            , test "distribute 3x2"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.by 3 2) CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ [ "A", "B", "C", "D", "E", "F" ]
                        , [ "G", "H", "I", "J" ]
                        ]
            , test "distribute 2x3"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.by 2 3) CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ [ "A", "B", "C", "D", "E", "F" ]
                        , [ "G", "H", "I", "J" ]
                        ]
            ]
        , describe "distribute `auto`"
            [ test "distribute A to J"
                <| \_ ->
                    Expect.equal
                        (PS.distribute PS.auto CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I"]
                        , ["J"]
                        ]
            , test "distribute alphabet"
                <| \_ ->
                    Expect.equal
                        (PS.distribute PS.auto CS.single P.first alphabet
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I"]
                        , ["J","K","L","M","N","O","P","Q","R"]
                        , ["S","T","U","V","W","X","Y","Z"]
                        ]
            ]
        , describe "distribute `exact`"
            [ test "distribute A to J over exactly 3 pages"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.exact 3) CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D"]
                        , ["E","F","G","H"]
                        , ["I","J"]
                        ]
            , test "distribute alphabet over exactly 3 pages"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.exact 3) CS.single P.first alphabet
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I","J"]
                        , ["K","L","M","N","O","P","Q","R","S","T"]
                        , ["U","V","W","X","Y","Z"]
                        ]
            ]
        , describe "distribute by `rows`"
            [ test "distribute A to J by 2 rows"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.rows 2) CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I"]
                        , ["J"]
                        ]
            , test "distribute alphabet"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.rows 2) CS.single P.first alphabet
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I"]
                        , ["J","K","L","M","N","O","P","Q","R"]
                        , ["S","T","U","V","W","X","Y","Z"]
                        ]
            ]
        , describe "distribute by `cols`"
             [ test "distribute A to J"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.cols 2) CS.single P.first aToJ
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I"]
                        , ["J"]
                        ]
            , test "distribute alphabet"
                <| \_ ->
                    Expect.equal
                        (PS.distribute (PS.cols 2) CS.single P.first alphabet
                            |> Tuple.first
                            |> P.toList
                        )
                        [ ["A","B","C","D","E","F","G","H","I"]
                        , ["J","K","L","M","N","O","P","Q","R"]
                        , ["S","T","U","V","W","X","Y","Z"]
                        ]
            ]
        ]
