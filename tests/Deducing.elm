module Deducing exposing (..)

import Json.Encode as E

import Tron exposing (Tron)
import Tron.Core as Core
import Tron.Tree as T exposing (..)
import Tron.Tree.Build.Unit as B


import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "deducing"

{-

    describe "deducing"
        [ test "deduces path to the int control @ root"
            <| \_ ->
                Core.tryDeduce
                    tree
                    { path = [ 0 ], value = E.int 18 }
                    -- { path = [ (0, "foo") ], value = E.int 18 }
                    |> Expect.equal
                            (Just
                                { path = [ 0 ]
                                -- { path = [ (0, "foo") ]
                                , value = E.int 18
                                , type_ = "slider"
                                })

        , test "deduces path to the int control @ nest"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "nest", "aaa" ], value = E.int 18 }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 1, 0 ]
                                , value = E.int 18
                                , type_ = "slider"
                                })

        , test "deduces path to the button control @ nest"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "nest", "bar" ], value = E.null }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 1, 1 ]
                                , value = E.null
                                , type_ = "button"
                                })

        , test "deduces path to the choice control @ root"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "choice" ], value = E.int 1 }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 2 ]
                                , value = E.int 1
                                , type_ = "choice"
                                })

        , test "deduces path to the choice control @ nest"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "nest", "choice-in" ], value = E.int 2 }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 1, 2 ]
                                , value = E.int 2
                                , type_ = "choice"
                                })


        , test "converts string value to its ID value @ root"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "choice" ], value = E.string "B" }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 2 ]
                                , value = E.int 1
                                , type_ = "choice"
                                })

        , test "converts string value to its ID value @ nest"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "nest", "choice-in" ], value = E.string "C" }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 1, 2 ]
                                , value = E.int 2
                                , type_ = "choice"
                                })

        , test "deduces path to the toggle control @ root"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "fooA" ], value = E.bool True }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 3 ]
                                , value = E.bool True
                                , type_ = "toggle"
                                })

        , test "deduces path to the text control @ root"
            <| \_ ->
                Core.tryDeduce
                    { path = [ "apple" ], value = E.string "pear" }
                    tree
                    |> Expect.equal
                            (Just
                                { path = [ 4 ]
                                , value = E.string "pear"
                                , type_ = "text"
                                })

        ] -}


tree : Tree ()
tree =
    B.root
        [
            ( "foo"
            , B.int { min = 0, max = 100, step = 1 } 10
            )
        ,
            ( "nest"
            , B.nest
                [
                    ( "aaa"
                    , B.int { min = 0, max = 100, step = 1 } 10
                    )
                ,
                    ( "bar"
                    , B.button
                    )

                ,
                    ( "choice-in"
                    , B.choice
                        ([ "A", "B", "C" ]
                            |> B.buttons
                                |> B.toSet identity
                        )
                        "C"
                    )

                ]
            )
        ,
            ( "choice"
            , B.choice
                ([ "A", "B", "C" ]
                    |> B.buttons
                        |> B.toSet identity
                )
                "C"
            )
        ,
            ( "fooA"
            , B.toggle False
            )
        ,
            ( "apple"
            , B.text "apple"
            )
        ]