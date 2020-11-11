module Default.Gui exposing (..)


import Color exposing (Color)

import Gui exposing (Gui)
import Gui.Build as Gui
import Gui.Property  exposing (Property)
import Gui.Property as Property exposing (Label)
import Gui.Render.Style exposing (CellShape(..))


import Default.Model exposing (..)
import Default.Msg exposing (..)


for : Model -> Property Msg
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
                Gui.choice
                    ( 1, 4 )
                    Full
                    choiceToLabel
                    choices
                    model.choice
                    compareChoices
                    Choose )
        , ( "nest",
                nestedButtons model.buttonPressed
                -- allControlsNest model
          )
        , ( "button",
                Gui.buttonWith (Gui.icon "export") (always NoOp)
          )
        , ( "toggle",
                Gui.toggle
                    model.toggle
                    Switch
          )
        ]


nestedButtons : Choice -> Property Msg
nestedButtons curChoice =
    Gui.nest
        ( 2, 3 )
        Full
        [ ( "a", Gui.button <| always <| Pressed A )
        , ( "b", Gui.button <| always <| Pressed B )
        , ( "c", Gui.button <| always <| Pressed C )
        , ( "d", Gui.button <| always <| Pressed D )
        , ( "color", colorNest )
        ]


colorNest : Property Msg
colorNest =
    let
        colorCompKnob msg =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
                msg
    in
        Gui.nest
            ( 1, 3 )
            Full
            [ ( "red", colorCompKnob ChangeRed )
            , ( "green", colorCompKnob ChangeGreen )
            , ( "blue", colorCompKnob ChangeBlue )
            ]


allControlsNest : Model -> Property Msg
allControlsNest model =
    let
        colorCompKnob msg =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
                msg
    in
        Gui.nest
            ( 3, 4 )
            Full
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
                    Gui.choice
                        ( 1, 4 )
                        Full
                        choiceToLabel
                        choices
                        model.choice
                        compareChoices
                        Choose )
            , ( "nest",
                    nestedButtons model.buttonPressed
            )
            , ( "button",
                    Gui.buttonWith (Gui.icon "export") (always NoOp)
            )
            , ( "toggle",
                    Gui.toggle
                        model.toggle
                        Switch
            )
            , ( "sqbutton",
                    Gui.button (always NoOp)
            )
            ]


choiceToLabel : Choice -> Property.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"
