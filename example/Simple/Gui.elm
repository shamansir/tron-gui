module Simple.Gui exposing (..)


import Gui.Alt as Gui
import Gui.Alt exposing (Gui)


import Simple.Model exposing (..)
import Simple.Msg exposing (..)


for : Model -> Gui Msg
for model =
    Gui.make
        [ ( "ghost", Gui.ghost )
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
                    (Just model.choice)
                    compareChoices
                    Choose )
        , ( "nest",
                Gui.nest
                    Gui.Expanded
                    <| nestedButtons model.buttonPressed )
        , ( "toggle",
                Gui.toggle
                    (Gui.boolToToggle model.toggle)
                    (Gui.toggleToBool >> Switch) )
        ]


nestedButtons : Choice -> Gui Msg
nestedButtons curChoice =
    Gui.make
        [ ( "a", Gui.button <| always <| Pressed A )
        , ( "b", Gui.button <| always <| Pressed B )
        , ( "c", Gui.button <| always <| Pressed C )
        , ( "d", Gui.button <| always <| Pressed D )
        ]


choiceToLabel : Choice -> Gui.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"
