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
* choice control — a panel with options, or a click-through button, or a knob;
* group control — a panel with any other controls;

Group and choice controls support _pagination_ if the items doesn't fit;

The whole interface or its parts can be _"detached"_ to another browser window or even device. This feature requires a simple WebSocket server though, so it's optional, but we provide a full example of such feature; see it buy running `./example/start-example.sh Detachable` with `elm-live`, the sources for this example are in `example/Detachable` folder.

The encoders and decoders for JSON are included, so all the structure can be transferred to JavaScript, the tree itself and all the values updates are easily encoded back and forth as well, this gives you the ability to easily replace the rendered GUI with some JS implementation, [`dat.gui`](https://github.com/dataarts/dat.gui), for example — while keeping the structure define in typed Elm and your messages connected.

Next major features planned are:

* multi-choice control;
* controls with the shapes of 1x0.5, 2x2, 0.5x1 and so on, so that it would be easier to create complex panels; shapes are already there in API, but they are still experimental;
* support for `a-frame` and rendering to it, which would allow operate Tron in VR; see `AFrame` example for current status;

See `TODO.md` for more.

See `CHANGELOG.md` for the list of all the changes through versions.

Huge thanks to @imilman for the design/UX and priceless help through development.

## Tutorial

See [TUTORIAL](https://github.com/shamansir/tron-gui/blob/main/Tutorial.md) for the detailed guide on using Tron. 

<!-- 
## Screenshots

<img src="https://raw.githubusercontent.com/shamansir/tron-gui/79875cc096b0c16c669c8b83dca8d6e5593433fa/tron-example-light.png" width="549" height="497" alt="Light Theme" />

<img src="https://raw.githubusercontent.com/shamansir/tron-gui/79875cc096b0c16c669c8b83dca8d6e5593433fa/tron-example-dark.png" width="549" height="515" alt="Dark Theme" /> 

-->

## Adding to your Elm application

_Tron_ provides the `WithTron` helper which wraps the core `Browser....` functions and requests you to define the same
`init`, `view`, `update`, etc., as you usually define, with only few additions:

- your GUI description as `for : Model -> Tron Msg` function, where `Msg` is your usual message, see the examples below;
- copy and add `src/Tron.css` to your application, or refer to the one from the GitHub;
- what _theme_ (dark/light) it will have and where it would be _docked_ (in any corner or in the center);
- specify the way Tron communicates with JS, if it needs to, but usually the communication is off — but, if you want _detachable_ functionality, you'll need to start WebSocket server, all the required code is provided for that and requires minimum actions;

* [`WithTron`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/WithTron) documentation;
* [`Tron`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron) documentation;
* [`Tron.Build`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Build) documentation;
* [`Tron.Option.Render`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Option) documentation;
* [`Tron.Option.Communication`](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Option) documentation;

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
* `BuldFromJs` — an example of building the complete UI from JavaScript;
* `ListenFromJs` — listen for updates from JS;
* `ReportToJs` — report values ;

**NB**: The _Tron_ GUI is not designed to support all the above features at once, so please consider that `Everything` is not a good example of using Tron API, while _all others_ for sure are.

## Docker

* `docker build . -t tron-example`
* `docker run -p 8080:8080 tron-example`

Soon, there will be the ability to run specific example in Docker using environment variable.

# Run tests

* `elm-test`

## `WithTron` Examples

### One Knob

From `example/Example/OneKnob/Main.elm`, similar to `example/Example/Basic/Main.elm`:

```elm
import WithTron
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock

main : WithTron.Program () Model Msg
main =
    WithTron.element
        (Render.toHtml Dock.center Theme.dark)
        Communication.none
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
import WithTron
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Tree.Expose.Data as Exp

main : WithTron.Program () Example.Model Example.Msg
main =
    WithTron.element
        (Render.toHtml Dock.middleRight Theme.dark)
        (Communication.sendJson
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


port sendUpdate : Exp.Out -> Cmd msg

port initGui : Exp.Tree -> Cmd msg
```

### Detachable

From `example/Example/Detachable/Main.elm`:

```elm
import WithTron
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Tree.Expose.Data as Exp


main : WithTron.Program () Example.Model Example.Msg
main =
    WithTron.application
        (Render.toHtml Dock.center Theme.light)
        (Communication.detachable
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


port receieveUpdateFromWs : (Exp.In -> msg) -> Sub msg

port sendUpdateToWs : Exp.Out -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
```

## GUI definition Examples

Here are the examples of the interface definitions:

From `example/Example/Default/Gui.elm`:

### Default

```elm
import Tron exposing (Tron)
import Tron.Build as Tron


for : Model -> Tron Msg
for model =
    Tron.root
        [ ( "ghost", Tron.none )
        , ( "int",
                Tron.int
                    { min = -20, max = 20, step = 5 }
                    model.int
                    ChangeInt )
        , ( "float",
                Tron.float
                    { min = -10.5, max = 10.5, step = 0.5 }
                    model.float
                    ChangeFloat )
        , ( "xy",
                Tron.xy
                    ( { min = -20, max = 20, step = 5 }
                    , { min = -20, max = 20, step = 5 }
                    )
                    model.xy
                    ChangeXY )
        , ( "text",
                Tron.text
                    model.string
                    ChangeString )
        , ( "color",
                Tron.color
                    model.color
                    ChangeColor )
        , ( "choice",
                Tron.choice
                    choiceToLabel
                    choices
                    model.choice
                    compareChoices
                    Choose )
        , ( "nest",
                nestedButtons model.buttonPressed
          )
        , ( "button",
                Tron.button
                    (Tron.face <| Tron.iconAt [ "assets", "export.svg" ])
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
    Tron.nest
        [ ( "a", Gui.button <| always <| Pressed A )
        , ( "b", Gui.button <| always <| Pressed B )
        , ( "c", Gui.button <| always <| Pressed C )
        , ( "d", Gui.button <| always <| Pressed D )
        , ( "color", colorNest )
        ]
        |> Tron.shape (Tron.rows 2)

colorNest : Tron Msg
colorNest =
    let
        colorCompKnob msg =
            Gui.float
                { min = 0, max = 255, step = 1 }
                0
                msg
    in
        Tron.nest
            [ ( "red", colorCompKnob ChangeRed )
            , ( "green", colorCompKnob ChangeGreen )
            , ( "blue", colorCompKnob ChangeBlue )
            ]
            |> Tron.shape (Tron.cols 1)            


choiceToLabel : Choice -> Path.Label
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
import Tron.Build as Tron


for : Model -> Tron Msg
for model =
    Tron.root
        [
            ( "honk on"
            ,
                Tron.toggle
                    (Tuple.first model.honk)
                    (\v -> if v then HonkOn else HonkOff)
            )
        ,
            ( "honk"
            ,
                if Tuple.first model.honk then
                    Tuple.second model.honk
                        |> honkGui
                        |> Tron.expand
                else Tron.none
            )
        ,
            ( "eye"
            , eyeGui model.eye
            )
        ,
            ( "look at"
            ,
                Tron.choice
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
                Tron.toggle
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
                Tron.toggle
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
    Tron.nest
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
        |> Tron.shape (Tron.cols 2)


eyeGui : EyeConfig -> Tron Msg
eyeGui config =
    Gui.nest
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
        |> Tron.shape (Tron.cols 1)        


colorsGui : Bool -> Colors -> Tron Msg
colorsGui isPunk colors =
    Gui.nest
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
        |> Tron.shape (Tron.cols 2)           
```

## Special Builders

When you don't have any _messages_ or you want to define GUI only to pass it to JavaScript side, you may use other Builders which don't require specifying messages and convert their values automatically using `Tree.map`:

- `Tron.Tree.Build.Unit` which provides `Tree ()`;
- `Tron.Tree.Build.Any` which provides `Tree a`;

### `Tron ()` example

```elm
import Tron exposing (Tree)
import Tron.Tree.Build.Unit as Tron


gui : Tree
gui =
    Tron.root
        [ ( "ghost", Tron.none )
        ,
            ( "int"
            ,
                Tron.int
                    { min = -20, max = 20, step = 5 }
                    0
            )
        ,
            ( "float"
            ,
                Tron.float
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
                Tron.choice
                    choiceToLabel
                    choices
                    A
                    compareChoices
                    |> Tron.expand
                    |> Tron.shape (Tron.cols 3)
            )
        ,
            ( "nest", nestedButtons C )
        ,
            ( "button"
            ,
                Tron.button
                    <| Tron.face
                    <| Tron.themedIconAt
                        (\theme ->
                            [ "assets", "export_" ++ Theme.toString theme ++ ".svg" ]
                        )
            )

        ,
            ( "toggle", Tron.toggle False )
        ]


nestedButtons : Choice -> Tree 
nestedButtons curChoice =
    Tron.nest
        [ ( "a", Tron.button )
        , ( "b", Tron.button )
        , ( "c", Tron.button )
        , ( "d", Tron.button )
        , ( "color", colorNest )
        ]
        |> Tron.shape (Tron.cols 3)


colorNest : Tree
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
            |> Tron.shape (Tron.cols 2)
```
