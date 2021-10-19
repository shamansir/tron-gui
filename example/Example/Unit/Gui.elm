module Example.Unit.Gui exposing (..)


import Color
import Url.Builder as Url

import Tron exposing (Tron)
import Tron.Builder.Unit as Gui
import Tron.Tree as Tree
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape exposing (..)
import Tron.Style.Theme as Theme


type Choice
    = A | B | C | D | E | F | G
    | H | I | J | K | L | M | N | O | P
    | Q | R | S | T | U | V | W | X | Y | Z


choices : List Choice
choices = [ A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z ]


gui : Tron ()
gui =
    Gui.root
        [ ( "ghost", Gui.none )
        ,
            ( "int"
            , Gui.int
                    { min = -20, max = 20, step = 5 }
                    0
            )
        ,
            ( "float"
            , Gui.float
                { min = -10.5, max = 10.5, step = 0.5 }
                0.0
            )
        ,
            ( "xy",
                Gui.xy
                    ( { min = -20, max = 20, step = 5 }
                    , { min = -20, max = 20, step = 5 }
                    )
                    ( 0, 0 )
            )
        ,
            ( "text"
            , Gui.text "foobar"
            )
        ,
            ( "color",
                Gui.color
                    <| Color.rgb255 255 194 0
            )
        ,
            ( "choice",
                Gui.choiceBy
                    (choices
                        |> Gui.buttons
                        |> Gui.addLabels choiceToLabel
                    )
                    A
                    compareChoices
                    |> Gui.expand
                    |> Gui.shape (cols 3)
            )
        ,
            ( "nest", nestedButtons C )
        ,
            ( "button"
            , Gui.button
                |> Gui.face (
                        Gui.themedIconAt
                            (\theme ->
                                [ "assets", "export_" ++ Theme.toString theme ++ ".svg" ]
                            )
                )
            )

        ,
            ( "toggle", Gui.toggle False )
        ]


nestedButtons : Choice -> Tron ()
nestedButtons curChoice =
    Gui.nest
        [ ( "a", Gui.button )
        , ( "b", Gui.button )
        , ( "c", Gui.button )
        , ( "d", Gui.button )
        , ( "color", colorNest )
        ]
        |> Gui.shape (cols 2)


colorNest : Tron ()
colorNest =
    let
        colorCompKnob =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
    in
        Gui.nest
            [ ( "red", colorCompKnob )
            , ( "green", colorCompKnob )
            , ( "blue", colorCompKnob )
            ]
            |> Gui.shape (cols 1)


choiceToLabel : Choice -> Tree.Label
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
