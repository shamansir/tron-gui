# Tron GUI


[`tron-gui`](https://package.elm-lang.org/packages/shamansir/tron-gui/) (the package in Elm package library).

Tron GUI is a minimal interface for your web application.

It provides the friendly API for you to easily describe your interface using a mimimal number of smartly picked and designed controls and by nesting them.

For the moment, the provided controls are:

* number knob, with step;
* XY control;
* text control;
* color control;
* toggle control;
* button, can have some icon or none;
* choice control — a panel with options;
* group control — a panel with any other controls;

The whole interface or its parts can be "detached" to another browser window or even device. This feature requires a simple WebSocket server though, so it's optional, but we provide a full example of such feature.

The encoders and decoders for JSON are included, so all the structure can be transferred to JavaScript, all the values updates are easily encoded back and forth as well, this gives you the ability to easily replace the GUI with some JS implementation, like [`dat.gui`](https://github.com/dataarts/dat.gui), for example.



Here's the example of the interface description:


```elm
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
        ]


nestedButtons : Choice -> Property Msg
nestedButtons curChoice =
    Gui.nestIn
        ( rows 2 )
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
        Gui.nestIn
            ( cols 1 )
            Full
            [ ( "red", colorCompKnob ChangeRed )
            , ( "green", colorCompKnob ChangeGreen )
            , ( "blue", colorCompKnob ChangeBlue )
            ]


choiceToLabel : Choice -> Property.Label
choiceToLabel c =
    case c of
        A -> "The A"
        B -> "The B"
        C -> "The C"
        D -> "The D"
```


## Examples

See `example/` folder for more examples:

* `example/Basic` is just the interface for the above description;
* `example/Detachable` the demo of how the interface can be detached at any nesting (if you run the provided WebScocket server before running the example);
* `example/DatGui` is about how easy it is to replace Tron GUI with `dat.gui`;
* `example/Everything` — all of the above, plus randomly generating interface structures;
