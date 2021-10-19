module Example.Default.Gui exposing (..)


import Color exposing (Color)

import Tron.OfValue exposing (Tron)
import Tron.Builder as Gui
import Tron.Tree  exposing (Tree)
import Tron.Tree as Tree
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape exposing (..)
import Tron.Style.Theme as Theme


import Example.Default.Model exposing (..)
import Example.Default.Msg exposing (..)


for : Model -> Tron Msg
for model =
    Gui.root
        [ ( "ghost", Gui.none )
        , ( "int",
                Gui.int
                    { min = -20, max = 20, step = 5 }
                    model.int
                    ChangeInt )
        , ( "float",
                Gui.float
                    { min = -10.5, max = 10.5, step = 0.5 }
                    model.float
                    ChangeFloat )
        , ( "xy",
                Gui.xy
                    ( { min = -20, max = 20, step = 5 }
                    , { min = -20, max = 20, step = 5 }
                    )
                    model.xy
                    ChangeXY )
        , ( "text",
                Gui.text
                    model.string
                    ChangeString )
        , ( "color",
                Gui.color
                    model.color
                    ChangeColor )
        , ( "choice",
                Gui.choiceBy
                    (choices
                        |> Gui.buttons
                        |> Gui.addLabels choiceToLabel
                    )
                    model.choice
                    compareChoices
                    Choose
                |> Gui.shape (cols 1))
        , ( "nest",
                nestedButtons model.buttonPressed
                -- allControlsNest model
          )
        , ( "button",
                Gui.button
                    (always NoOp)
                |> Gui.face exportIcon

          )
        , ( "toggle",
                Gui.toggle
                    model.toggle
                    Switch
          )
        ]


nestedButtons : Choice -> Tron Msg
nestedButtons curChoice =
    Gui.nest
        [ ( "a", Gui.button <| always <| Pressed A )
        , ( "b", Gui.button <| always <| Pressed B )
        , ( "c", Gui.button <| always <| Pressed C )
        , ( "d", Gui.button <| always <| Pressed D )
        , ( "color", colorNest )
        ] |> Gui.shape (cols 2)


colorNest : Tron Msg
colorNest =
    let
        colorCompKnob msg =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
                msg
    in
        Gui.nest
            [ ( "red", colorCompKnob ChangeRed )
            , ( "green", colorCompKnob ChangeGreen )
            , ( "blue", colorCompKnob ChangeBlue )
            ] |> Gui.shape (cols 1)


allControlsNest : Model -> Tron Msg
allControlsNest model =
    let
        colorCompKnob msg =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
                msg
    in
        Gui.nest
            [ ( "ghost", Gui.none )
            , ( "int",
                    Gui.int
                        { min = -20, max = 20, step = 5 }
                        model.int
                        ChangeInt )
            , ( "float",
                    Gui.float
                        { min = -10.5, max = 10.5, step = 0.5 }
                        model.float
                        ChangeFloat )
            , ( "xy",
                    Gui.xy
                        ( { min = -20, max = 20, step = 5 }
                        , { min = -20, max = 20, step = 5 }
                        )
                        model.xy
                        ChangeXY )
            , ( "text",
                    Gui.text
                        model.string
                        ChangeString )
            , ( "color",
                    Gui.color
                        model.color
                        ChangeColor )
            , ( "choice",
                    Gui.choiceBy
                        (choices
                            |> Gui.buttons
                            |> Gui.addLabels choiceToLabel
                        )
                        model.choice
                        compareChoices
                        Choose
                        |> Gui.shape (cols 1))
            , ( "nest",
                    nestedButtons model.buttonPressed
            )
            , ( "button",
                    Gui.button
                        (always NoOp)
                    |> Gui.face exportIcon
            )
            , ( "toggle",
                    Gui.toggle
                        model.toggle
                        Switch
            )
            , ( "sqbutton",
                    Gui.button (always NoOp)
            )
            ] |> Gui.shape (rows 4)


choiceToLabel : Choice -> Tree.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"


exportIcon : Gui.Face
exportIcon =
    Gui.themedIconAt
        (\theme ->
            [ "assets", "export_" ++ Theme.toString theme ++ ".svg" ]
        )
