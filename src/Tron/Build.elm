module Tron.Build exposing
    ( Set
    , root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool, button, buttonWith
    , nest, choice, choiceBy, strings, labels, palette, buttons
    , face, Face, Icon, icon, iconAt, themedIcon, themedIconAt, useColor
    , live, toChoice, toSet, mapSet, handleWith, toSwitch, toKnob
    , expand, collapse, shape, cells
    )


{-| The Builder helps you define the structure of your GUI. It is made as abstract from the representation as possible, so that any control could be changed to a better or alternative one without the requirement for you to change the definition. What you specify is the type of the value to be changed and the message to in the case of such change. Plus grouping, as deep as you want and as wide as the free space allows.

See [Tutorial](https://github.com/shamansir/tron-gui/blob/main/Tutorial.md) for the details on how to use it.

This way works the optional connection with `dat.gui`: `dat.gui` operates over the similar set of types of values — we can replace them with our controls and vice versa, groups are interchanged with folders. Of course, this definition also can be translated to plain HTML forms, as the set of possible `<input>`’s is limited to the same set of types.

Every control may:

* have a setup to define how it behaves, for example min/max values and step for knobs;
* have a default/current value, which is usually just taken from your model, from a correspodning field;
* have a handler, which receives the new value and produces your message with it;
* have a String label;
* have a shape, which could be different from 1x1 in the grid;
* be expandable to a group of any controls, contained in some shape;
* if it's a group or nesting or choice, it can be detached if GUI configured so;

# Shapes

For `choice...` and `nest...` items you may specify both the shape of the panel (in cells) and shape of every cell (they only can be all of one shape).

Shape of the panel may be requested to be calculated automatically (`Shape.auto`) or asked to have the desired number of rows (`Shape.rows 3`) or columns (`Shape.columns 1`) and so auto-calculate the other side, or specified manually using `by` function (`Shape.by 4 3`).

Cell shapes are usually just 1x1 (`CellShape.single`), but in some specific cases you may want controls to be twice smaller (`CellShape.half`) or horizontal (`CellShape.oneByTwice`) or vertical (`CellShape.twiceByOne`). Some controls take completely another form when their cell shape is changed.

# The Naming

In the examples below, we use `Build.` as a prefix in the places where we reference functions from this module. This assumes that you did something like this in your code:

    import Tron exposing (Tron)
    import Tron.Build as Build exposing (..)

However, it is ok to use any name you like, for sure. Be it `Tron.` or `Gui.` or `Def.` or whatever...

# Defining your interface

To define the structure of your interface, you need to have the function with this type:

    for : Model -> Tron Msg

Where `Msg` is the message of your application.

This module contains all the helpers you need to build your own interface. Here is the excerpt from `OneKnob` example:

    type alias Amount = Float


    type Msg
        = AmountChanged Amount


    type alias Model = Amount


    for : Model -> Tron Msg
    for amount =
        Build.root
            [
                ( "amount"
                , Build.float
                    { min = 0, max = 1, step = 0.01 }
                    amount
                    AmountChanged
                )
            ]

`Tron msg` is the type that represents any cell in your GUI. If it's a nesting, it also contains recursively other instances of `Tron msg`.

Use the methods in the module as the helpers in building your own grid structure:

    Build.root
        [ ( "int", Build.int ... )
        , ( "float", Build.float ... )
        ,
            ( "nest"
            , Build.nest
                [ ( "button", Build.button ... )
                , ...
                ]
            )
        ]

Using `Tron msg` together with `Tron.map` you may build your GUI from several modules with different messages.

    Build.root
        [ ( "one", ModuleOne.gui model.moduleOne |> Tron.map ModuleOne )
        , ( "two", ModuleTwo.gui model.moduleTwo |> Tron.map ModuleTwo )
        , ( "three", ModuleThree.gui model.moduleThree |> Tron.map ModuleThree )
        ]

For more information, see the `examples` folder in the source code.

# Sets

`Set msg` is just the list of components' definitions together with their labels. It is what
`Build.root`, `Build.nest` and `Build.choice` get as an argument. `Set msg` is exposed as a separate type to help you in the cases where you build your GUI from several modules, but want to join them in a single panel rather than nesting every module separately.

    Build.nest
        <| (ModuleOne.gui |> List.map (Tuple.mapSecond <| Tron.map ToModuleOne))
            ++ (ModuleTwo.gui |> List.map (Tuple.mapSecond <| Tron.map ToModuleTwo))
            ++ (ModuleThree.gui |> List.map (Tuple.mapSecond <| Tron.map ToModuleThree))

# Sets
@docs Set, mapSet, toSet

# Root
@docs root

# Items
@docs none, int, float, number, xy, coord, color, text, input, button, buttonWith, toggle, bool

# Groups
@docs nest, choice, choiceBy, strings, labels, palette

# Buttons
@docs buttons, useColor, face, Face

# Icons
@docs Icon, icon, iconAt, themedIcon, themedIconAt

# Force expand / collapse for nesting
@docs expand, collapse

# Shape
@docs shape, cells

# Live

Usually in your `for` function you set the default value to the control, but if you change the control with `live`, then you'll be able to pass some dynamic value from your model to it.

@docs live

# Conversion between types of controls + helpers
@docs toChoice, toKnob, toSwitch, handleWith
-}


import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)
import Dict
import Url exposing (Url)

import Tron as Def exposing (Tron)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Control.Value as Value exposing (..)
import Tron.Tree.Internals exposing (..)
import Tron.Tree.Internals as Tree
import Tron.Tree.Controls as Tree exposing (expand)
import Tron.Control exposing (Control(..))
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape exposing (rows, cols)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Button as Button exposing (Face(..), Icon(..))
import Tron.Control.Impl.Toggle
import Tron.Control.Impl.Nest as Nest exposing (Form(..), ItemId)

import Tron.Tree.Build.Any as B
import Tron.Tree.Build.Choice as Choice


{-|
See also: `Build.Set`.
-}
type alias Tron msg = Def.Tron msg


{-| `Set msg` is just the list of components' definitions together with their labels. It is what
`Build.root` and `Build.nest` get as an argument. `Set msg` is exposed as a separate type to help you in the cases where you build your GUI from several modules, but want to join them in a single panel rather than nesting every module separately.

    Build.nest
        <| (ModuleOne.gui |> List.map (Tuple.mapSecond <| Build.map ToModuleOne))
            ++ (ModuleTwo.gui |> List.map (Tuple.mapSecond <| Build.map ToModuleTwo))
            ++ (ModuleThree.gui |> List.map (Tuple.mapSecond <| Build.map ToModuleThree))

See also: `Build.map`.
-}
type alias Set msg = List ( Label, Tron msg )


type alias Label = Path.Label


{-| map all the items in the set with one function. -}
mapSet : (a -> b) -> Set a -> Set b
mapSet =
    List.map << Tuple.mapSecond << Def.map


{-| Similar to `Cmd.none`, `Sub.none` etc., makes it easier to use expressions in the definition.

For example:

    if user |> User.is Root then
        Build.button RemoveAllFiles
    else
        Build.none

-}
none : Tron msg
none = B.none <| always Nothing


{-| Use the root only once, to mark the first visible row of your UI, and put anything else inside.

Actually it is just an alias for the nested row of controls, always expanded.

    for myModel =
        Build.root
            [
                ( "octave"
                , Build.int { min = 1, max = 8, step = 1 } myModel.octave ChangeOctave
                )
            ,
                ( "note"
                , Build.int { min = 1, max = 127, step = 1 } myModel.midiNote ChangeMidiNote
                )
            ,
                ( "shape"
                , Build.nest

                    [
                        ( "sine"
                        , Build.button
                            (always <| ChangeShape Sine)
                            |> Build.face
                                (Build.icon <| Url.Build.relative [ "sawwave.svg" ] [])

                        )
                    ,
                        ( "square"
                        , Build.button
                            (always <| ChangeShape Square)
                            |> Build.face
                                (Build.icon <| Url.Build.relative [ "sawwave.svg" ] [])
                        )
                    ,
                        ( "saw"
                        , Build.button
                            (always <| ChangeShape Saw)
                            |> Build.face
                                (Build.icon <| Url.Build.relative [ "sawwave.svg" ] [])

                        )
                    ]

                )
            ]

-}
root : Set msg -> Tron msg
root props =
    nest
        props
        |> expand
        |> shape (rows 1)


{-| `float` creates a control over a rational number value, with a minimum, maximum and a step.

    Build.float { min = 0, max = 44000, step = 1 } myModel.frequency ChangeFrequency

-}
float : Axis -> Float -> ( Float -> msg ) -> Tron msg
float axis value toMsg =
    B.float axis value <| Value.fromNumber >> Maybe.map toMsg


{-| `int` creates a control over an integer number value, with a minimum, maximum and a step.

    Build.int { min = 1, max = 8, step = 1 } myModel.octave ChangeOctave

-}
int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Tron msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


{-| `number` is the alias for `Builde.float`.

    Build.number { min = 0, max = 44000, step = 1 } myModel.frequency ChangeFrequency
-}
number : Axis -> Float -> ( Float -> msg ) -> Tron msg
number = float


{-| `xy` creates a control over a pair of two number values or anything that can be translated to them.

    Build.xy
        ( { min = 0, max = scene.width, step = 0.1 }
        , { min = 0, max = scene.height, step = 0.1 }
        )
        myModel.lightDirection
        PointLightTo

-}
xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Tron msg
xy axes value toMsg =
    B.xy axes value <| Value.fromXY >> Maybe.map toMsg


{-| `coord` is the alias for `Build.xy`
-}
coord : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Tron msg
coord = xy


{-| `input` creates a control over a value which can be translated to `String` and parsed from `String`. It is just a helper over `text` control.

    Build.input
        (\color ->
            case color of
                Red -> "red"
                Green -> "green"
                Blue -> "blue"
        )
        (\val ->
            case val of
                "red" -> Just Red
                "green" -> Just Green
                "blue" -> Just Blue
                _ -> Nothing
        )
        model.selectedColor
        ChangeColor
-}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Tron msg
input toString fromString value toMsg =
    B.text (toString value) <| Value.fromText >> Maybe.andThen fromString >> Maybe.map toMsg


{-| `text` creates a control over a `String` value.

    Build.text model.elfName RenameElf
-}
text : String -> (String -> msg) -> Tron msg
text value toMsg =
    B.text value <| Value.fromText >> Maybe.map toMsg


{-| `color` creates a control over a color, for the moment it is Hue/Saturation in 2D space, same as `xy`, but with different representation, but we may improve it later. Or you may change it to `choice` with your own palette.

The `Color` type here is from `avh4/elm-color` module.

    Build.color model.lightColor AdjustColor
-}
color : Color -> (Color -> msg) -> Tron msg
color value toMsg =
    B.color value <| Value.fromColor >> Maybe.map toMsg


{-| `button` creates a control over a _unit_ `()` value. Type science aside, when you receive the unit value `()` in the handler, it just means that this button was pushed.

    Build.button <| always DoABang
-}
button : (() -> msg) -> Tron msg
button =
    buttonWith Title


{-| -}
type alias Face = Button.Face


{-| Set face for the `button`, `nest`, or `choice`, it can be icon or color:

    Build.button (always DoABang)
        |> Build.face (Build.iconAt [ "assets", "myIcon.svg" ])

    Build.nest
        ... |> Build.face (Build.iconAt [ "assets", "myIcon.svg" ])

    Build.choice
        ... |> Build.face (Build.iconAt [ "assets", "myIcon.svg" ])

    Build.buttons ...
        |> Tron.map (Build.face << Build.icon)

-}
face : Face -> Tron msg -> Tron msg
face =
    Tree.setFace


{-| -}
type alias Icon = Button.Icon


{-| Create an `Icon` from its URL or filename.

    import Url.Builder as Url

    Build.icon
        <| makeUrl <| Url.relative [ "assets", "myicon.svg" ] []

See also: `Build.iconAt`, `Build.themedIcon`, `Build.themedIconAt`
-}
icon : Url -> Face
icon = Button.icon >> WithIcon


{-| Create an `Icon` using its relative local path.

    Build.iconAt [ "assets", "myicon.svg" ]

See also: `Build.themedIconAt`
-}
iconAt : List String -> Face
iconAt = Button.iconAt >> WithIcon


{-| Create a themed `Icon` from its URL or filename.

    import Url.Builder as Url

    Build.themedIcon
        <| \theme ->
            makeUrl <| Url.relative [ "assets", "myicon_" ++ Theme.toString theme ++ ".svg" ] []
-}
themedIcon : (Theme -> Maybe Url) -> Face
themedIcon = Button.themedIcon >> WithIcon


{-| Create a themed `Icon` using its relative local path.

    Build.themedIconAt
        <| \theme -> [ "assets", "myicon_" ++ Theme.toString theme ++ ".svg" ]
-}
themedIconAt : (Theme -> List String) -> Face
themedIconAt = Button.themedIconAt >> WithIcon


{-| Create a button with a given face. Use `icon`, `iconAt`, `themedIcon`, `themedIconAt` to create faces.
-}
buttonWith : Face -> (() -> msg) -> Tron msg
buttonWith face_ toMsg =
    B.buttonWith face_ <| Value.fromAction >> Maybe.map toMsg


{-| `toggle` creates a control over a boolean value.

    Build.toggle model.lightOn SwitchLight
-}
toggle : Bool -> (Bool -> msg) -> Tron msg
toggle value toMsg =
    B.toggle value <| Value.fromToggle >> Maybe.map toMsg


{-| `bool` is the alias for `Build.toggle`
-}
bool : Bool -> (Bool -> msg) -> Tron msg
bool = toggle


{-| `nest` lets you group other controls (including other `nest`ings) under a button which expands a group. Also, this group can be _detached_ if GUI is confugured so.

Handler receives the state of the group, like if it is exapanded or collapsed or detached, but usually it's fine just to make it `always NoOp`.

    Build.nest
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

See also: `Style.Shape`, `Style.CellShape`
-}
nest : Set msg -> Tron msg
nest items =
    Group
        Nothing
        Tree.defaultNestShape
        <| Control
            ( Array.fromList items
            )
            { form = Collapsed
            , face = Nothing
            , page = 0
            }
            <| always Nothing


{-| Create a button face representing a color:

    Build.button (always NoOp) |> Build.face (Build.useColor Color.green)

    [ Color.white, Color.red, Color.yellow ]
        |> Build.buttons
        |> List.map (Tron.with (Build.face << Build.useColor))
        |> Build.toSet Color.colorToHexWithAlpha
-}
useColor : Color -> Face
useColor = WithColor


{-| `choice` defines a list of options for user to choose between. Consider it as `<select>` tag with `<option>`s. When some option is chosen by user, the handler gets the corresponding value. Notice that we ask for `comparable` type here.

    Build.choice
        ([ 128, 256, 512 ]
            |> Build.buttons
            |> Build.toSet String.fromInt
        )
        model.bitrate
        ChangeBitrate

*NB*: If you don't want to use `comparable` types, but rather want to specify you own compare function, use `choiceBy`.

*NB*: If you want to add icons to the buttons, use `buttons |> List.map (Tron.map (face << myIcon))`, where `myIcon : a -> Face`, for colors use `[ Color.white, Color.red, Color.yellow, ... ] |> buttons |> List.map (Tron.map (face << useColor))`.

See also: `Build.choiceBy`, `Build.strings`, `Build.palette`, `Style.Shape`, `Style.CellShape`
-}
choice
     : Set comparable
    -> comparable
    -> ( comparable -> msg )
    -> Tron msg
choice set current toMsg =
    Choice.helperDef
        Tree.defaultNestShape
        set
        current
        (==)
        <| always toMsg


{-| `choiceBy` is identical to `choice`, but asks user for a custom comparison function instead of requiring `comparable` values.

    Build.choiceBy
        ([ Sine, Square, Triangle, Saw ]
            |> Build.buttons
            |> Build. waveToString
        )
        model.waveShape
        compareWaves -- sometimes just (==) works, but it's better not to rely on it
        ChangeWaveShape

See also: `Build.strings`, `Build.palette`, `Style.Shape`, `Style.CellShape`
-}
choiceBy
     : Set a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Tron msg
choiceBy set current compare toMsg =
    Choice.helperDef
        Tree.defaultNestShape
        set
        current
        compare
        <| always toMsg


{-| `strings` is a helper to create `choice` over string values.

    Build.strings
        greekChildrenNames
        model.currentChildName
        ChangeChildName
-}
strings
     : List String
    -> String
    -> ( String -> msg )
    -> Tron msg
strings options current toMsg =
    choice
        (options
            |> buttons
            |> toSet identity
        )
        current
        toMsg
    |> shape (cols 1)
    |> cells CS.twiceByHalf


{-| `labels` is a helper to create `choice` over the values that could be converted to string/labels and compared using those.

Requires a message that is a fallback for a case when comparison failed.
-}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> msg
    -> ( a -> msg )
    -> Tron msg
labels toLabel options current fallback toMsg =
    let
        labelToValue =
            options
                |> List.map (\v -> ( toLabel v, v ) )
                |> Dict.fromList
    in choice
        (options
            |> List.map toLabel
            |> buttons
            |> toSet identity
        )
        (toLabel current)
        (\label ->
            labelToValue
                |> Dict.get label
                |> Maybe.map toMsg
                |> Maybe.withDefault fallback
        )
        |> shape (cols 1)
        |> cells CS.twiceByHalf


{-| `palette` is a helper to create `choice` over color values.

    Build.palette
        [ Color.aqua, Color.rouge, Color.vanilla ]
        model.iceCreamColor
        RepaintIceCream
-}
palette
     : List ( Label, Color )
    -> Color
    -> (Color -> msg)
    -> Tron msg
palette options current toMsg =
    choiceBy
        (options
            |> buttons
            |> List.map (Def.with (face << useColor << Tuple.second))
            |> toSet Tuple.first
            |> Def.mapSet Tuple.second
        )
        current
        (\cv1 cv2 ->
            case ( cv1 |> Color.toRgba, cv2 |> Color.toRgba ) of
                ( c1, c2 ) ->
                    (c1.red == c2.red) &&
                    (c1.blue == c2.blue) &&
                    (c1.green == c2.green) &&
                    (c1.alpha == c2.alpha)
        )
        toMsg
    |> cells CS.half


{-| `buttons` is the helper to translate a list of anything to a list of buttons.

It could be useful to pass such list to `choice` or `nest`:

    Build.choiceBy
        ([ Sine, Square, Triangle, Saw ]
            |> Build.buttons
            |> Build.toSet waveToString
        )
        model.waveShape
        compareWaves -- sometimes just (==) works, but it's better not to rely on it
        ChangeWaveShape

Or:

    Build.nest
        ([ Sine, Square, Triangle, Saw ]
            |> Build.buttons
            |> Build.toSet waveToString -- `toSet` is just another name for `addLabels`
            |> Build.handleWith ChangeWaveShape
        )

-}
buttons : List a -> List (Tron a)
buttons =
    List.map (button << always)



{-| Convert a list of components to a set by adding labels.
-}
toSet : (a -> Label) -> List (Tron a) -> Set a
toSet toLabel =
    List.map
        (\prop ->
            Tree.get prop
                |> (\handler ->
                        ( Tree.getValue prop
                                |> handler
                                |> Maybe.map toLabel
                                |> Maybe.withDefault "-"
                        , prop
                        )
                    )
        )
    -->> List.filterMap identity


{-| Forcefully expand the nesting:

    Build.nest ... |> Build.expand
    Build.choice ... |> Build.expand
-}
expand : Tron msg -> Tron msg
expand = Tree.expand


{-| Forcefully collapse the nesting:

    Build.nest ... |> Build.collapse
    Build.choice ... |> Build.collapse
-}
collapse : Tron msg -> Tron msg
collapse = Tree.collapse


{- Set the icon to the control that can have it:

    Build.nest ... |> Build.face (Build.icon ...)
    Build.button ... |> Build.face (Build.icon ...)
-}
-- TODO
-- setIcon : Icon -> Tron msg -> Tron msg
-- setIcon icon = Tree.collapse


{-| Convert a `nest` control to `choice` control. It can be done
easily by specifying a handler:

    Build.nest
        ([ Sine, Square, Triangle, Saw ]
            |> buttons
            |> toSet waveToString -- `toSet` is just another name for `addLabels`
            |> handleWith (always NoOp)
        )
    |> toChoice ChangeShapeById
-}
toChoice : (ItemId -> msg) -> Tron msg -> Tron msg
toChoice f =
    B.toChoice
        >> Tree.map
            (always <| Value.fromChoice >> Maybe.map (Tuple.first >> f))


{-| Convert choice control to a switch by click form:

    Build.choice ... |> Build.toSwitch
-}
toSwitch : Tron msg -> Tron msg
toSwitch =
    Tree.setChoiceMode Nest.SwitchThrough


{-| Convert choice control to the knob form:

    Build.choice ... |> Build.toKnob
-}
toKnob : Tron msg -> Tron msg
toKnob =
    Tree.setChoiceMode Nest.Knob


{-| Convert any control to update its value live (i.e. on every change take them from you model)

    Build.knob ... |> Build.live
-}
live : Tron msg -> Tron msg
live =
    Tree.Live


{-| Handle a set of items with a converter of item to a message

    Build.nest
        ([ Sine, Square, Triangle, Saw ]
            |> Build.buttons
            |> Build.toSet waveToString -- `toSet` is just another name for `addLabels`
            |> Build.handleWith ChangeWaveShape
        )

Alias for `Tron.mapSet`
-}
handleWith : (a -> msg) -> Set a -> Set msg
handleWith = Def.mapSet


{-| Changes panel shape for `nest` and `choice` panels:

    Build.nest ... |> Buidler.shape (CS.cols 2)

    Build.choice ... |> Buidler.shape (CS.rows 1)

    Build.choice ... |> Buidler.shape (CS.by 2 3)
-}
shape : PanelShape -> Tron msg -> Tron msg
shape = Tree.setPanelShape


{-| Changes cell shape for `nest` and `choice` panels:

    Build.nest ... |> Buidler.cells PS.single

    Build.choice ... |> Buidler.shape PS.halfByTwo

    Build.choice ... |> Buidler.shape PS.halfByHalf
-}
cells : CellShape -> Tron msg -> Tron msg
cells = Tree.setCellShape