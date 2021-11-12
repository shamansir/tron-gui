# Tron. Introduction

Tron is the web UI for any graphical web playground, such as processing.js sketches, WebGL, [hydra](https://hydra-book.glitch.me/), shaders, etc., powered by the advantages of the Elm language. If your application is written in Elm, it is native to plug the Tron interface in. If it’s in JavaScript, we provide the friendly JS API (written with the help of TypeScript), which makes it also quite easy to define your UI and plug it from outside.

We in [Computational Arts Initiative](https://cai.jetbrains.com) use Tron to control our generative graphics and tools.

// TODO: examples of the usage.

# Tron. The Idea

The core idea of Tron project is to have UI controls laid out in the rectangular grid and at the same time be structured as a tree.

_On the pictures you see only 1x1 controls, but under the hood, anything like 2x1 or 1x2 is also supported._

Another feature is the ability to _detach_ the complete UI or just some parts of the interface to another browser or tab or device or whatever so that it wouldn’t take the precious space.

In the core, UI is serialisable to JSON back and forth, it gives us the ability to send it to JS or receive it from there, send it over WebSockets or to have the UI constructor storing it in your LocalStorage.

The feature we keep in mind, but not yet developed, is to support VR rendering of the UI, with some custom UX, of course.

And yes, of course, its design is inspired by the Tron movie. Which one of those two, you decide.

For now, we have some controls prepared for you:

* Button, for user to press;
* The number, integer or float, either as a slider or some kind of knob;
* XY, for the coordinates;
* Color control, to make things saturated;
* Toggle, to switch something on or off;
* Text control, to enter any bullshit;
* Nest, to group any number of controls in a panel, also supports paging;
* Choice, to pretend to give user the options to choose from;

These controls usually have different modes, for example the Choice control could look like the panel with many buttons, or the knob, to switch it between options, or to be the switch-button itself, to click the options through. We’ll cover the details further in the documentation;

# Tron. Installation

Installing Tron in your project is easy.

First, ensure to have the Elm language installed, either by installing it [from binary](https://guide.elm-lang.org/install/elm.html), or:

```bash
npm install -g elm
```

If what you have is JS project, initialise the Elm application in it:

```bash
elm init
```

In any way, add the Tron package to your project:

```bash
elm install shamansir/tron-gui
```

# Tron. Plugging in

## Elm way

The project with Tron UI has six pillars to build it over (because it is much more stable that if it had just three, don’t you think?):

### The Pillars

#### Pillar I. `Tron.Tree a`

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Tree))

`Tron.Tree a`, is the complete tree structure of your controls , and every control or nesting in it holds the value of type `a` inside; the latter fact is usually irrelevant to you, but just so you know.

#### Pillar II. `Tron msg`

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron))

`Tron msg` which is the same as `Tree (Control.Value -> msg)`, so it is the `Tree` where every control may produce some particular message in response to its current value;


#### Pillar III. `for` function

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/WithTron))

`for` function, which we kindly ask you to define, produces `Tron msg` from the model of your program and also gets the previous state of the controls represented as `Tree ()` in case you would ever need those previous values;

```haskell
for : Tree () -> model -> Tron msg
```


#### Pillar IV. `Build`

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Build))

`Build` module helps you build the structure of your interface, we have one for [`Tron msg`](https://package.elm-lang.org/packages/shamansir/tron-gui/13.0.0/Tron-Build), one for [`Tree ()`](https://package.elm-lang.org/packages/shamansir/tron-gui/13.0.0/Tron-Tree-Build-Unit) and one for [`Tree a`](https://package.elm-lang.org/packages/shamansir/tron-gui/13.0.0/Tron-Tree-Build-Any) , but please stick to the `Tron.Build` one;

#### Pillar V. `WithTron.*`

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/WithTron))

`WithTron` helpers over `Browser.*` definitions, they help you to attach your Tron interface to the application with common `init`, `update`, `view` and `subscriptions` , but also these functions get the latest state of the `Tree` for your pleasure;

#### Pillar VI. `WithTron.ValueAt`

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/WithTron-ValueAt))

`WithTron.ValueAt` helpers which allow you to extract previous values from the previous `Tree ()` when you need them, they take the path to the control and return `Maybe x` , where `x` is `Float` for numbers, `( Float, Float )` for XY, `( Path.Label, Int )` for choices & so on;

### Applying the pillars.

In short, it is:

* Choose `WithTron.*` helper which fits you;
    * Specify where to dock and the theme you like;
    * Add communication by taste;
* Structure your interface (with the help of `Tron.Build` and optionally `WithTron.ValueAt`) by your implementation of `for : Model -> Tree -> Tron Msg`;
* Proceed with `init` / `update` / `view` / … as usual;
* …That’s it!
* … … Oh, and please don’t forget to copy `Tron.css` to your application from `./src`, we truly hope it is a temporary and minor inconvenience;

In long:

To add Tron to your application, choose one of `WithTron.application`, `WithTron.element`, `WithTron.document` or `WithTron.sandbox` functions.

They are similar to the `Browser.*` helpers they wrap, with a few subtle differences:

* You are asked to specify `for` function, which is expected to return `Tron msg` (built with the help of `Tron.Build.*` functions);
* Every function except `init` gets `Tree ()` as the previous state of the UI — use `WithTron.ValueAt.*` helpers if you want to extract the previous values from UI (for example, to show/hide some parts of the UI on the toggle);

See `Main.elm` in any of the `example/*` folders for the reference.

Here is the code from `OneKnob/Main.elm`, which uses `.sandbox`, because it does not depend on the past:

```haskell
type alias Amount = Float


type Msg
    = AmountChanged Amount


type alias Model = Amount


init : Model
init = 0


for : Model -> Tron Msg
for amount =
    Builder.root
        [
            ( "amount"
            , Builder.float
                { min = 0, max = 1, step = 0.01 }
                amount
                AmountChanged
            )
        ]


view : Model -> Html Msg
view amount =
    Html.text
        <| String.fromFloat amount


update : Msg -> Model -> Model
update msg curAmount =
    case msg of

        AmountChanged newAmount ->
            newAmount


main : WithTron.Program () Model Msg
main =
    WithTron.sandbox
        (Render.toHtml Dock.center Theme.dark)
        { for = for
        , init = init
        , view = view
        , update = update
        }
```

Would it use `.element` with the same set of functions, it would look like this:

```haskell
WithTron.element
        (Render.toHtml Dock.center Theme.dark)
        (Communication.none)
        { for = always for
        , init = always ( init, Cmd.none )
        , view = always view
        , update = \msg _ model
                -> ( update msg model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        }
```

#### Render targets

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Option-Render))

Wether you want the UI be hidden or render it to Html or to VR (it’s not yet working well, but it’s in the development, so the option is there), or show the Debug version of Tron, this is where you decide.

Docking could be performed to any corner or side of the screen, even its center.

Theme indeed may be `Theme.dark` or `Theme.light`. Someday some `Theme.nostalgic` will also be there.

#### Communication

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Option-Communication))

Communication with JS is done using ports, if you don’t want to connect to JS in any way (don’t tell it to the folks reading the other paragraph), just set it to `Communication.none`.

For instance, if you want to receive all the values updates from Tron in JS, you would use `Communication.sendJson` and define `ack` and `transmit` ports.

Or, if you would want to make your UI detachable, you would use `Communication.detachable` and start the provided WebSocket server somewhere locally.

See the examples in documentation and in the `examples/` directory.

#### Building your UI

To build your interface, define how it depends on your `Model` in your `for : Model -> Tree () -> Tron Msg` function.

It is quite easy, with the help of `Tron.Build as Tron` . As with `Html msg` or `Svg msg`, it produces `Tron msg` which is changed to the exact type of your message as `Tron Msg`.

Just remember to always start with `Tron.root`:

```haskell
for : Model -> Tree -> Tron Msg
for model _ =
    Tron.root
        [ ( "control-1", ... control-1 ... )
        , ( "control-2", ... control-2 ... )
        , ( "control-3", ... control-3 ... )
        , ...
        ]
```

In all the examples below we use:

```haskell
import Tron.Build as Tron
```

##### Nothings

Even though you’ve read nothing about adding controls yet, we already took a look into the future and we’re sure this is what you need to know.

No doubts, in some future you would like to hide some control, or even a group of controls, if something, in the model or in the previous state of UI, is against showing it.

To let you do it, there is `Tron.none` helper, same as with `Cmd.none` or `Sub.none`, you may use it in conditionals or cases or any language constructs:

```haskell
if not user.isAdmin then
    Tron.none
else
    Tron.button |> Tron.withFace Color.red
```

##### Buttons

Button is the simplest thing, it just produces some message on click:

```haskell
type Msg = OhMyGoshIAmPushed

Tron.button <| always OhMyGoshIAmPushed
```

###### Many faces of the button

By default the button only shows its label.

The buttons have a feature of having icons though, if you want to assign some, use one of `Tron.icon`, `Tron.iconAt`,`Tron.themedIcon`, or `Tron.themedIconAt`:

```haskell
Tron.button ...
    |> Tron.face
        (Tron.iconAt [ "assets", "subfolder", "smiley.png" ])

```

Or, if your icon depends on the current theme:

```haskell
Tron.button ...
    |> Tron.face
        (Tron.themedIconAt
            \theme ->
                [ "assets"
                , case theme of
                    Theme.Dark -> "dark"
                    Theme.Light -> "light"
                , "smiley.png"
                ]
        )
```

The button may have a coloured circle over it:

```haskell
Tron.button ...
    |> Tron.face (Tron.useColor Color.magenta)
```

##### Numbers

Given the `type Msg = OneSmallStep Int`:

```haskell
Tron.int { min = 0, max = 100, step = 2 } 50 OneSmallStep
```

Creates the knob with the minimum value of `0` and the maximum of `100`, and only even values to switch between, and `50` as the current value (you may take one from your model), when user’s mouse stops dragging the knob at some point, the corresponding value is sent with the message of `OneSmallStep`.

If you would like to produce the message even _during_ the dragging process, just redirect it to `Tron.live`:

```haskell
Tron.int {...} ... OneSmallStep |> Tron.live
```

The `Tron.float` function lets you define the knob producing the floating point numbers:

```haskell
Tron.float { min = -1.0, max = 5.0, step = 0.5 } 0.0 OneSmallFloatingStep
```

##### Coordinates

Now we have two axes to define, although it is not very different from what we did with numbers:

```haskell
type Msg = MoveTo ( Float, Float )

Tron.xy
    ( { min = -1.0, max = 5.0, step = 0.5 }
    , { min = 0.0, max = 10.0, step = 0.2 }
    )
    ( 0.0, 0.0 )
    MoveTo
```

##### Color

Adding colour control is also quite easy:

```haskell
import Color as C

type Msg = ChangeColorOfMySadness C.Color

Tron.color C.magenta ChangeColorOfMySadness
```

##### Text

Text controls are also not as tough to add:

```haskell
type Msg = SaveHokku String

Tron.text "Your hokku" SaveHokku
```

##### Toggle

As well as toggles:

```haskell
type Msg = ToggleFreedomOfSpeech Bool

Tron.toggle False ToggleFreedomOfSpeech
```

##### _Live controls_

Some controls like _knobs_ don’t send any values to your `update` function while being dragged, by default—only when user releases the mouse or trackpad, the last value is sent into the application cycle; If you want to receive values even while user drags the value, just convert them to `.live` controls:

```haskell
Tron.int ... |> Tron.live

Tron.float ... |> Tron.live

Tron.xy ... |> Tron.live
Tron.choice ... |> Tron.asSwitch |> Tron.live
```

##### _Nested controls_

Nested controls hold and/or operate several components inside. They’re usually panels with other controls inside.

[`Tron.nest`](#Nest) and [`Tron.choice`](#Choice) are the only implementations of nested components for the moment, considering `Tron.root` being a custom `Tron.nest` under the hood, and `Tron.buttons` & `Tron.labels` & `Tron.palette` being the helpers over `Tron.choice`.

Since any nesting or choice is hidden under a button, you may change its face (see _Many faces of the button_):

```haskell
Build.nest ...
    |> Tron.face (Tron.iconAt [ .... ])

Build.choice ...
    |> Tron.face (Tron.useColor Color.magenta)
```

###### _Sets_

Sets are just labels paired with corresponding controls:

```haskell
type alias Set msg = List ( String, Tron msg )
```

You are requested to pass a set of controls to any nesting control, such as `Tron.root`, `Tron.nest` or `Tron.choice`.

There is a helper to covert a list of controls holding some value to the `Set`:

```haskell
Tron.toSet : (a -> String) -> List (Tron a) -> Set a
```

Also, there is a `Tron.mapSet` helper to convert sets to different types:

```haskell
Tron.mapSet : (a -> b) -> Set a -> Set b
```

These helpers could be useful, for example, when you define a list of buttons based on some list of values:

```haskell
TODO
```

###### _Panel shapes_

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Style-PanelShape))

Panel shape is how many cells the panel takes in the GUI grid (it is automatically calculated considering the fact that cells inside could be halflings or giants, see _Cell shape_ below).

You are not required to specify both sides, just use `.rows` or ``.`cols`` helpers to say how many rows or columns you want in the panel and other side will be calculated automatically. Or even use `.auto` and both sides will be suggested, but this usually doesn't look good. To specify both sides manually, use `.by`.

```haskell
import Tron.Style.PanelShape as PS

Tron.nest ... |> Tron.shape (PS.by 5 6)
Tron.nest ... |> Tron.shape (PS.cols 3)
Tron.nest ... |> Tron.shape (PS.rows 2)
Tron.choice ... |> Tron.shape (PS.rows 2)
Tron.choice ... |> Tron.shape (PS.by 3 3 |> PS.manyPages)
Tron.nest ... |> Tron.shape (PS.by 5 5 |> PS.singlePage)
```

###### _Cell shapes_

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Style-CellShape))

For now, all the cells (controls) on one panel should have the same shape. But, it could be any supported shape and inner panels may have any other cell shape. So, cell shape is defined for the whole panel:

```
import Tron.Style.CellShape as CS

Tron.nest ... |> Tron.cells CS.<the-shape-you-want>

Tron.nest ... |> Tron.cells CS.single

Tron.choice ... |> Tron.cells CS.halfByOne
```

Sometimes controls change the way they look with different shapes.

Considering the default shape as 1x1 (`single`), the meaning of each value is:

* `.single` — `1x1`
* `.half` — `0.5x0.5`
* `.halfByOne` — `0.5x1`
* `.oneByHalf` — `1x0.5`
* `.twiceByHalf` — `2x0.5`
* `.halfByTwice` — `0.5x2`
* `.twiceByTwice` - `2x2`

###### _Pagination_

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Style-PanelShape#distribute))

If the items you’ve added to the panel don’t fit the requested panel shape (see _Panel Shapes_ above), they’re split into pages. But you may force pagination to be disabled:

```haskell
import Tron.Style.PanelShape as PS

Tron.nest ... |> Tron.shape (PS.by 5 5 |> PS.singlePage)

Tron.choice ... |> Tron.shape (PS.cols 6 |> PS.singlePage)
```

###### Expanding

If you want to expand or collapse a panel, just do it:

```haskell
Tron.nest ... |> Tron.expand
Tron.nest ... |> Tron.collapse
```

###### Nest

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Build#nest))

Nesting is just the separation of the controls in the panel with no special logic:

```haskell
type Component = Red | Green | Blue

type Msg = AdjustColor Component Float

Tron.nest
    [
        ( "red"
        , Build.float { min = 0, max = 255, step = 0.1 } model.red <| AdjustColor Red
        )
    ,
        ( "green"
        , Build.float { min = 0, max = 255, step = 0.1 } model.blue <| AdjustColor Green
        )
    ,
        ( "blue"
        , Build.float { min = 0, max = 255, step = 0.1 } model.blue <| AdjustColor Blue
        )
    ]
```

However, you may use the helpers like `.buttons` to fill the panels with the buttons.

```haskell
type WaveShape = Sine | Square | Triangle | Saw
type Msg = ChangeWaveShape WaveShape

waveToLabel : WaveShape -> String
waveToLabel = ...

Tron.nest
    ([ Sine, Square, Triangle, Saw ]
        |> Tron.buttons
        |> Tron.toSet waveToLabel
        |> Tron.handleWith ChangeWaveShape -- no "global message" as with `choice`
    )
```


###### Choice

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/Tron-Build#choice))

Choice is a special nesting panel (not obligatory a panel, see below) which sends the value of the control clicked inside this panel with the message. Since it shows its current value on the button that expands the panel, all the controls inside have to be buttons:

```haskell
type Msg = ChangeBitrate Int

Tron.choice
    ([ 128, 256, 512 ]
        |> Tron.buttons
        |> Tron.toSet String.fromInt
    )
    model.bitrate
    ChangeBitrate
```

But that also gives you power to add icons or controls there, just as with separate buttons (see _Many faces of the button_):

```haskell
productIcon : Product -> Tron.Face
productIcon product =
    Tron.iconAt
       [ "assets"
       , "icons"
       , Product.getName product ++ ".svg"
       ]


Tron.choiceBy
    (Product.all
        |> List.filter Product.hasIcon
        |> Tron.buttons
        |> List.map (Tron.with (Tron.face << productIcon))
        |> Tron.toSet Product.getName
    )
    Product.default
    Product.compare
|> Tron.shape (rows 3)
```

There are several wrappers over `Tron.choice` to help you create choice controls with specific types:

Any enumeration (requires comparison function since you know… Elm has no typeclasses :) ):

```haskell
type WaveShape = Sine | Square | Triangle | Saw
type Msg = ChangeWaveShape WaveShape

waveToLabel : WaveShape -> String
waveToLabel = ...

Tron.choiceBy
    ([ Sine, Square, Triangle, Saw ]
        |> Tron.buttons
        |> Tron.toSet waveToLabel
    )
    model.waveShape
    compareWaves -- sometimes just (==) works, but it's better not to rely on it
    ChangeWaveShape
```

For lists of strings:

```haskell
greekChildrenNames = [ '...', '...', ... ]

type Msg = SuggestChildName String

Tron.strings
    greekChildrenNames
    model.currentChildName
    SuggestChildName
```

For lists of colours:

```haskell
type Msg = RepaintIceCream Color

Tron.palette
    [ Color.aqua, Color.rouge, Color.vanilla ]
    model.iceCreamColor
    RepaintIceCream
```

Additionally, choice is not restricted to be a panel of things, it may also be represented as a button to switch between options (since they’re buttons inside):

```haskell
Tron.choice ... |> Tron.toSwitch
```

Or as a knob to switch between values (showing labels):

```haskell
Tron.choice ... |> Tron.toKnob
```

Also, any nest with buttons can be converted to the choice:

```haskell
type WaveShape = Sine | Square | Triangle | Saw
type Msg = ChangeWaveShape WaveShape | NoOp


waveToLabel : WaveShape -> String
waveToLabel = ...

Tron.nest
    ([ Sine, Square, Triangle, Saw ]
        |> Tron.buttons
        |> Tron.toSet waveToString
        |> Tron.handleWith (always NoOp)
    )
|> Tron.toChoice ChangeShapeById
```

#### Using `ValueAt`

([docs](https://package.elm-lang.org/packages/shamansir/tron-gui/latest/WithTron-ValueAt))

In your `for`, `update`, `subscriptions` and `view` functions, you get the previous state of the tree as `Tree ()`, the values are there, and the tools to extract them are the helpers in the `ValueAt` module.

They are all meant to have similar format:

```haskell
<control-name> : List Label -> Tree () -> Maybe <type-of-value>
```

Where `List Label` is the path to the control in the tree and `Tree ()` is current tree, and `Maybe` holds the value if some control was found at the path **and** its type matches the requested value. I.e. even if there is a _toggle_ at the given path, but you’ve asked for the value from a _knob_, `Maybe` won’t hold a number, since it is impossible to convert one to another.

So there are several `Decoder`s for every control that have the similar signature for you to use;

To get some value from the previous state of a control, use `ask` function, pass the corresponding decoder to it, then pass the path and the current state of the tree:

```haskell
tree |> ask (toggle [ "Goose", "Punk" ]) -- returns `Maybe Bool`
tree |> ask (choice [ "Color Scheme", "Product" ]) -- returns `Maybe (ItemId, Path.Label)`
tree |> ask (choiceOf Products.all [ "Color Scheme", "Product" ]) -- returns `Maybe Product`
-- NB: Just ensure to use the very same list you used for creating the `choice` in this case
tree |> ask (color [ "Feather", "Color" ]) -- returns `Maybe Color`
    -- and so on...
```

*NB*: Notice that for `choiceOf` it is important to pass the very same list of values you passed to the corresponding control _last time_. Usually there should be no problem here, but if in your `for` function you have filtered some values from the choice in a different way than on the previous iteration, just remember that you are asking the _previous_ tree for the value, where choice values were filtered differently.

## JavaScript way

### The interface is defined in Elm

TODO

```Elm


```

### You define the interface in JavaScript

TODO

# Tron. Follow ups

## Tron. Detachable.

To make your Tron interface detachable, you need to start the WebSocket server that receives JSONs with the tree parts and the updates.

TODO.

## Tron. Constructor.

# Tron. Development

TODO.
