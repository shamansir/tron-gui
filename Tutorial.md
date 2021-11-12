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

(docs)

`Tron.Tree a`, is the complete tree structure of your controls , and every control or nesting in it holds the value of type `a` inside; the latter fact is usually irrelevant to you, but just so you know.

#### Pillar II. `Tron msg`

(docs)

`Tron msg` which is the same as `Tree (Control.Value -> msg)`, so it is the `Tree` where every control may produce some particular message in response to its current value;


#### Pillar III. `for` function

(docs)

`for` function, which we kindly ask you to define, produces `Tron msg` from the model of your program and also gets the previous state of the controls represented as `Tree ()` in case you would ever need those previous values; 

```haskell
for : Tree () -> model -> Tron msg		
```


#### Pillar IV. `Builder`

(docs)

`Builder` that helps you build the structure of your interface, we have one for [`Tron msg`](https://package.elm-lang.org/packages/shamansir/tron-gui/13.0.0/Tron-Build), one for [`Tree ()`](https://package.elm-lang.org/packages/shamansir/tron-gui/13.0.0/Tron-Tree-Build-Unit) and one for [`Tree a`](https://package.elm-lang.org/packages/shamansir/tron-gui/13.0.0/Tron-Tree-Build-Any) ;

#### Pillar V. `WithTron.*`

(docs)

`WithTron` helpers over `Browser.*` definitions, they help you to attach your Tron interface to the application with common `init`, `update`, `view` and `subscriptions` , but also these functions get the latest state of the `Tree` for your pleasure;

#### Pillar VI. `WithTron.ValueAt`

(docs)

`WithTron.ValueAt` helpers which allow you to extract previous values from the previous `Tree ()` when you need them, they take the path to the control and return `Maybe x` , where `x` is `Float` for numbers, `( Float, Float )` for XY, `( Path.Label, Int )` for choices & so on;    

### Applying the pillars.

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

(docs)

Wether you want the UI be hidden or render it to Html or to VR (it’s not yet working well, but it’s in the development, so the option is there), or show the Debug version of Tron, this is where you decide.

Docking could be performed to any corner or side of the screen, even its center.

Theme indeed may be `Theme.dark` or `Theme.light`. Someday some `Theme.nostalgic` will also be there.

#### Communication

(docs)

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

The buttons have a feature of having icons though, if you want to assign some, use one of `Tron.icon`, `Tron.iconAt`,`Tron.themedIcon`, or `Tron.themedIconAt`:

```haskell
Tron.button ...
	|> Tron.iconAt [ "assets", "subfolder", "smiley.png" ]

-- or ...

Tron.button ...
	|> Tron.themedIconAt 
		\theme -> 
			[ "assets"
			, case theme of 
				Theme.Dark -> "dark" 
				Theme.Light -> "light" 
			, "smiley.png"
			]
```

###### Many buttons’ faces 

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

##### Nested controls

###### Sets

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

###### Panel shapes

(docs)

###### Cell shapes

(docs)

###### Nest

(docs)

```haskell
Tron.nest []
```

###### Choice

(docs)

Sjjjsj

#### Using `ValueAt`

## JavaScript way

```Elm
foo : Maybe a 

```
