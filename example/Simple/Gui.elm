module Simple.Gui exposing (..)


import Gui.Build as Gui
import Gui.Gui exposing (Gui)
import Gui.Property  exposing (Property)
import Gui.Property as Gui exposing (Label)


import Simple.Model exposing (..)
import Simple.Msg exposing (..)


for : Model -> Property Msg
for model =
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
                Gui.choice
                    choiceToLabel
                    choices
                    model.choice
                    compareChoices
                    Choose )
        , ( "nest",
                nestedButtons model.buttonPressed )
        , ( "toggle",
                Gui.toggle
                    (Gui.boolToToggle model.toggle)
                    (Gui.toggleToBool >> Switch) )
        ]
        (always NoOp)


nestedButtons : Choice -> Property Msg
nestedButtons curChoice =
    Gui.nest
        [ ( "a", Gui.button <| always <| Pressed A )
        , ( "b", Gui.button <| always <| Pressed B )
        , ( "c", Gui.button <| always <| Pressed C )
        , ( "d", Gui.button <| always <| Pressed D )
        ]
        (always NoOp)


choiceToLabel : Choice -> Gui.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"
