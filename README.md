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
* button, can have some icon or none, or be a color switch;
* choice control — a panel with options;
* group control — a panel with any other controls;

Group and choice controls support _pagination_ if the items doesn't fit;

The whole interface or its parts can be _"detached"_ to another browser window or even device. This feature requires a simple WebSocket server though, so it's optional, but we provide a full example of such feature; see it buy running `./example/start-example.sh Detachable` with `elm-live`, the sources for this example are in `example/Detachable` folder.

The encoders and decoders for JSON are included, so all the structure can be transferred to JavaScript, all the values updates are easily encoded back and forth as well, this gives you the ability to easily replace the rendered GUI with some JS implementation, [`dat.gui`](https://github.com/dataarts/dat.gui), for example — while keeping the structure define in typed Elm and your messages connected.

Next major features planned are:

* multi-choice control;
* design improvements;
* controls with the shapes of 1x0.5, 2x2, 0.5x1 and so on, so that it would be easier to create complex panels; shapes are already there in API, but they are still experimental;
* support for `a-frame` and rendering to it, which would allow operate Tron in VR; see `AFrame` example for current status;


## Screenshots

<img src="https://raw.githubusercontent.com/shamansir/tron-gui/79875cc096b0c16c669c8b83dca8d6e5593433fa/tron-example-light.png" width="549" height="497" alt="Light Theme" />

<img src="https://raw.githubusercontent.com/shamansir/tron-gui/79875cc096b0c16c669c8b83dca8d6e5593433fa/tron-example-dark.png" width="549" height="515" alt="Dark Theme" />

## Adding to your Elm application

_Tron_ provides the `WithTron` helper which wraps the core `Browser....` functions and requests you to define the same
`init`, `view`, `update`, etc., as you usually define, with only few additions:

- your GUI description as `for :: Model -> Tron Msg` function, where `Msg` is your usual message, see the examples below;
- the option to select _theme_ (dark/light) and _docking_ (in any corner or in the center);
- the way Tron communicates with JS, if it needs to, usually the communication is off — but, if you want _detachable_ functionality, you'll need to start WebSocket server, all the required code is provided for that and requires minimum actions;

* [`WithTron`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/WithTron) documentation;
* [`Tron`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron) documentation;
* [`Tron.Builder`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Builder) documentation;
* [`Tron.Option`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Option) documentation;

See some example usages of `WithTron` below.

## Complete Application Examples

See `example/` folder for the whole application examples.

There is the `start-example.sh` script that helps to run every one of them, just pass a name of the example you want to see as an argument:

* `Basic` — just the GUI and the Goose;
* `Everything` — all the features in one: switching themes, docking, random-generated interface, detachable, ... (NB: _see  note below_);
* `Detachable` — the parts of GUI may be detached to a separate tab (run `start-server.sh` first);
* `DatGui` — connecting to `dat.gui` using JS transfer;
* `OneKnob` — only one control and its value, nothing else; also gives the example of the minimal Elm application with Tron GUI, where everything is defined in one module (really doesn't require a lot of code!);
* `Random` — random interface by a click of a button;
* `AFrame` — render to virtual reality using A-Frame (currently, the early draft);
* `ReportToJsBacked` — an example to use when connecting Tron to JS application;
* `ReportToJsJson` — a demonstration of sending any value update to port as JSON, as well as the complete GUI structure;
* `ReportToJsString` — a demonstration of sending any value update to port as labeled path and string value;

**NB**: The _Tron_ GUI is not designed to support all the above features at once, so please consider that `Everything` is not a good example of using Tron API, while _all others_ for sure are.

## Docker

* `docker build . -t tron-example`
* `docker run -p 8080:8080 tron-example`

Soon, there will be the ability to run specific example in Docker using environment variable.

## `WithTron` Examples

### One Knob

From `example/Example/OneKnob/Main.elm`, similar to `example/Example/Basic/Main.elm`:

```elm
import WithTron exposing (ProgramWithTron)
import Tron.Option as Option
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock

main : ProgramWithTron () Model Msg
main =
    WithTron.element
        (Option.toHtml Dock.center Theme.dark)
        Option.noCommunication
        { for = for -- `for :: Model -> Tron msg`, see examples below
        , init = init -- your usual `init` function
        , view = view -- your usual `view` function
        , update = update -- your usual `update` function
        , subscriptions = subscriptions -- your usual `subscriptions` function
        }
```

### Report to JS

From `example/Example/ReportToJsJson/Main.elm`:

```elm
import WithTron exposing (ProgramWithTron)
import Tron.Option as Option
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock

main : ProgramWithTron () Example.Model Example.Msg
main =
    WithTron.element
        (Option.toHtml Dock.middleRight Theme.dark)
        (Option.sendJson
            { ack = initGui
            , transmit = sendUpdate
            }
        )
        { for = ExampleGui.for
        , init = always Example.init
        , view = always <| Html.div [] []
        , update = Example.update
        , subscriptions = always Sub.none
        }


port sendUpdate : Exp.RawOutUpdate -> Cmd msg

port initGui : Exp.RawProperty -> Cmd msg
```

### Detachable

From `example/Example/Detachable/Main.elm`:

```elm
import WithTron exposing (ProgramWithTron)
import Tron.Option as Option
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Builder as Builder

main : ProgramWithTron () Example.Model Example.Msg
main =
    WithTron.application
        (Option.toHtml Dock.center Theme.light)
        (Option.detachable
            { ack = ackToWs
            , transmit = sendUpdateToWs
            , receive = receieveUpdateFromWs identity
            }
        )
        { for = ExampleGui.for
        , init = always Example.init
        , view =
            \model ->
                { title = "Detachable Tron"
                , body = [ Example.view model ]
                }
        , update = Example.update
        , subscriptions = always Sub.none
        , onUrlChange = always Example.NoOp
        , onUrlRequest = always Example.NoOp
        }


port receieveUpdateFromWs : (Exp.RawInUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawOutUpdate -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
```

## GUI definition Examples

Here are the examples of the interface definitions:

From `example/Example/Default/Gui.elm`:

### Default

```elm
import Tron exposing (Tron)
import Tron.Builder as Gui


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
                Gui.buttonWith
                    (Gui.iconAt [ "assets", "export.svg" ])
                    (always NoOp)
          )
        , ( "toggle",
                Gui.toggle
                    model.toggle
                    Switch
          )
        ]


nestedButtons : Choice -> Tron Msg
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


colorNest : Tron Msg
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

### Goose

From `example/Example/Default/Goose.elm`:

```elm
import Tron exposing (Tron)
import Tron.Builder as Gui


for : Model -> Tron Msg
for model =
    Gui.root
        [
            ( "honk on"
            ,
                Gui.toggle
                    (Tuple.first model.honk)
                    (\v -> if v then HonkOn else HonkOff)
            )
        ,
            ( "honk"
            ,
                if Tuple.first model.honk then
                    Tuple.second model.honk
                        |> honkGui
                        |> Gui.expand
                else Gui.none
            )
        ,
            ( "eye"
            , eyeGui model.eye
            )
        ,
            ( "look at"
            ,
                Gui.choice
                    ( Shape.auto )
                    Cell.single
                    (\v ->
                        case v of
                            Left -> "left"
                            Right -> "right")
                    [ Left, Right ]
                    model.lookAt
                    (==)
                    LookAt
            )
        ,
            ( "punk on"
            ,
                Gui.toggle
                    model.punk
                    (\v -> if v then PunkOn else PunkOff)
            )
        ,
            ( "colors"
            , colorsGui model.punk model.colors
            )
        ,
            ( "boots on"
            ,
                Gui.toggle
                    (
                        case model.shoes of
                            None -> False
                            Boots -> True
                    )
                    (\v -> ChangeShoes <| if v then Boots else None)
            )
        ]


honkGui : HonkConfig -> Tron Msg
honkGui config =
    Gui.nest
        ( cols 2 )
        Cell.single
        [
            ( "position"
            ,
                let posAxis = { min = -50, max = 50, step = 1 }
                in Gui.xy
                    ( posAxis, posAxis )
                    config.position
                    ChangeHonkTextPosition
            )
        ,
            ( "size"
            ,
                Gui.text
                    (String.fromInt <| config.size)
                    (String.toInt
                        >> Maybe.withDefault (default.honk |> Tuple.second |> .size)
                        >> ChangeHonkTextSize)
            )
        ,
            ( "text"
            ,
                Gui.text
                    config.text
                    ChangeHonkText
            )
        ,
            ( "color"
            ,
                Gui.color
                    config.color
                    ChangeHonkTextColor
            )
        ]


eyeGui : EyeConfig -> Tron Msg
eyeGui config =
    Gui.nest
        ( cols 1 )
        Cell.single
        [
            ( "position"
            ,
                let posAxis = { min = -5, max = 5, step = 1 }
                in Gui.xy
                    ( posAxis, posAxis )
                    config.position
                    ChangeEyePosition
            )
        ,
            ( "size"
            ,
                Gui.number
                    { min = 1, max = 10, step = 0.1 }
                    config.size
                    ChangeEyeSize
            )
        ]


colorsGui : Bool -> Colors -> Tron Msg
colorsGui isPunk colors =
    Gui.nest
        ( cols 2 )
        Cell.single
        [ ( "eye", Gui.color colors.eye ChangeEyeColor )
        , ( "feathers", Gui.color colors.feathers ChangeFeathersColor )
        , ( "skin", Gui.color colors.skin ChangeSkinColor )
        , ( "background", Gui.color colors.background ChangeBackground )
        ,
            ( "iroquois"
            , if isPunk
                then Gui.color colors.iroquois ChangeIroquoisColor
                else Gui.none
            )
        ]
```

## Special Builders

When you don't have any _messages_ or you want to define GUI only to pass it to JavaScript side, you may use other Builders which don't require specifying messages and convert their values automatically:

- `Tron.Builder.Unit` which provides `Tron ()`;
- `Tron.Builder.String` which provides `Tron (List String, String)` and so converts any value to the pair of the labeled path (such as `"honk/color"`, in the case of the _Goose_ example) and stringified value;
- `Tron.Builder.Proxy` which provides `Tron ProxyValue` and so converts any value to the special `ProxyValue` which is a data type representing the type of value and its contents, such as `Color (Rgba 0.5 0.5 0.7 1.0)` or `XY -2.5 -2.5` & s.o.;

Using functions from `Tron.Expose.Convert`, any of these `Tron`s, or your own `Tron`, may be converted to `Tron (RawOutValue, msg)` or simlar, so that it will carry the port-&-JSON-friendly version of the value along with the messages. Use `Tron.map Tuple.first` on such, to ignore the messages at all.

The example with `Tron.Builder.Unit` (from `example/Example/Default/Goose.elm`):

*NB*: Notice that defining controls this way doesn't require you to specify any message, plus in this case it is just `Tron ()` rather than `Tron Msg`, but still it is easy to replace one with another just by changing import and adding/removing messages at the end of the calls:

### `Tron ()` example

```elm
import Tron exposing (Tron)
import Tron.Builder.Unit as Gui

gui : Tron ()
gui =
    Gui.root
        [ ( "ghost", Gui.none )
        ,
            ( "int"
            ,
                Gui.int
                    { min = -20, max = 20, step = 5 }
                    0
            )
        ,
            ( "float"
            ,
                Gui.float
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
                    ( cols 3 )
                    single
                    choiceToLabel
                    choices
                    A
                    compareChoices
                    |> Gui.expand
            )
        ,
            ( "nest", nestedButtons C )
        ,
            ( "button"
            ,
                Gui.buttonWith
                    <| Gui.themedIconAt
                        (\theme ->
                            [ "assets", "export_" ++ Theme.toString theme ++ ".svg" ]
                        )
            )

        ,
            ( "toggle", Gui.toggle False )
        ]


nestedButtons : Choice -> Tron ()
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


colorNest : Tron ()
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
```
