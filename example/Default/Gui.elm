module Default.Gui exposing (..)


import Gui.Build as Gui
import Gui.Gui exposing (Gui)
import Gui.Property  exposing (Property)
import Gui.Property as Property exposing (Label)


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
                    choiceToLabel
                    choices
                    model.choice
                    compareChoices
                    Choose )
        , ( "nest",
                nestedButtons model.buttonPressed )
        , ( "button",
                Gui.button1 (Gui.icon "export") (always NoOp)
          )
        , ( "toggle",
                Gui.toggle
                    (Property.boolToToggle model.toggle)
                    (Property.toggleToBool >> Switch) )
        ]
        (always NoOp)


nestedButtons : Choice -> Property Msg
nestedButtons curChoice =
    Gui.nestIn
        ( 2, 3 )
        [ ( "a", Gui.button <| always <| Pressed A )
        , ( "b", Gui.button <| always <| Pressed B )
        , ( "c", Gui.button <| always <| Pressed C )
        , ( "d", Gui.button <| always <| Pressed D )
        , ( "color", colorNest )
        ]
        (always NoOp)


colorNest : Property Msg
colorNest =
    let
        colorCompKnob =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
                (always NoOp)
    in
        Gui.nestIn
            ( 1, 3 )
            [ ( "red", colorCompKnob )
            , ( "green", colorCompKnob )
            , ( "blue", colorCompKnob )
            ]
            (always NoOp)


choiceToLabel : Choice -> Property.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"
