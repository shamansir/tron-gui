module Example.Unit.Gui exposing (..)


import Color exposing (Color)

import Tron exposing (Tron)
import Tron.Build.Unit as Gui
import Tron.Property  exposing (Property)
import Tron.Property as Property exposing (Label)
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape exposing (..)


type Choice = A | B | C | D


choices : List Choice
choices = [ A, B, C, D ]


gui : Gui.Builder
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
                Gui.choice
                    ( cols 1 )
                    single
                    choiceToLabel
                    choices
                    A
                    compareChoices
            )
        ,
            ( "nest", nestedButtons C )
        ,
            ( "button"
            , Gui.buttonWith (Gui.icon "export")
            )

        ,
            ( "toggle", Gui.toggle False )
        ]


nestedButtons : Choice -> Gui.Builder
nestedButtons curChoice =
    Gui.nest
        ( cols 2 )
        single
        [ ( "a", Gui.button )
        , ( "b", Gui.button )
        , ( "c", Gui.button )
        , ( "d", Gui.button )
        , ( "color", colorNest )
        ]


colorNest : Gui.Builder
colorNest =
    let
        colorCompKnob =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
    in
        Gui.nest
            ( cols 1 )
            single
            [ ( "red", colorCompKnob )
            , ( "green", colorCompKnob )
            , ( "blue", colorCompKnob )
            ]


choiceToLabel : Choice -> Property.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"


compareChoices : Choice -> Choice -> Bool
compareChoices cA cB =
    case ( cA, cB ) of
        ( A, A ) -> True
        ( B, B ) -> True
        ( C, C ) -> True
        ( D, D ) -> True
        _ -> False
