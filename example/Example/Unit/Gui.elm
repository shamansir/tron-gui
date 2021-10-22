module Example.Unit.Gui exposing (..)


import Color

import Tron.Tree.Build.Unit as Tron
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape exposing (..)
import Tron.Style.Theme as Theme


type Choice
    = A | B | C | D | E | F | G
    | H | I | J | K | L | M | N | O | P
    | Q | R | S | T | U | V | W | X | Y | Z


choices : List Choice
choices = [ A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z ]


gui : Tron.Tree
gui =
    Tron.root
        [ ( "ghost", Tron.none )
        ,
            ( "int"
            , Tron.int
                    { min = -20, max = 20, step = 5 }
                    0
            )
        ,
            ( "float"
            , Tron.float
                { min = -10.5, max = 10.5, step = 0.5 }
                0.0
            )
        ,
            ( "xy",
                Tron.xy
                    ( { min = -20, max = 20, step = 5 }
                    , { min = -20, max = 20, step = 5 }
                    )
                    ( 0, 0 )
            )
        ,
            ( "text"
            , Tron.text "foobar"
            )
        ,
            ( "color",
                Tron.color
                    <| Color.rgb255 255 194 0
            )
        ,
            ( "choice",
                Tron.choiceBy
                    (choices
                        |> Tron.buttons
                        |> Tron.toSet choiceToLabel
                    )
                    A
                    compareChoices
                    |> Tron.expand
                    |> Tron.shape (cols 3)
            )
        ,
            ( "nest", nestedButtons C )
        ,
            ( "button"
            , Tron.button
                |> Tron.face (
                        Tron.themedIconAt
                            (\theme ->
                                [ "assets", "export_" ++ Theme.toString theme ++ ".svg" ]
                            )
                )
            )

        ,
            ( "toggle", Tron.toggle False )
        ]


nestedButtons : Choice -> Tron.Tree
nestedButtons curChoice =
    Tron.nest
        [ ( "a", Tron.button )
        , ( "b", Tron.button )
        , ( "c", Tron.button )
        , ( "d", Tron.button )
        , ( "color", colorNest )
        ]
        |> Tron.shape (cols 2)


colorNest : Tron.Tree
colorNest =
    let
        colorCompKnob =
            Tron.float
                { min = 0, max = 255, step = 1 }
                0
    in
        Tron.nest
            [ ( "red", colorCompKnob )
            , ( "green", colorCompKnob )
            , ( "blue", colorCompKnob )
            ]
            |> Tron.shape (cols 1)


choiceToLabel : Choice -> Tron.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"
        E -> "The E"
        F -> "The F"
        G -> "The G"
        H -> "The H"
        I -> "The I"
        J -> "The J"
        K -> "The K"
        L -> "The L"
        M -> "The M"
        N -> "The N"
        O -> "The O"
        P -> "The P"
        Q -> "The Q"
        R -> "The R"
        S -> "The S"
        T -> "The T"
        U -> "The U"
        V -> "The V"
        W -> "The W"
        X -> "The X"
        Y -> "The Y"
        Z -> "The Z"


compareChoices : Choice -> Choice -> Bool
compareChoices cA cB =
    case ( cA, cB ) of
        ( A, A ) -> True
        ( B, B ) -> True
        ( C, C ) -> True
        ( D, D ) -> True
        _ -> False
