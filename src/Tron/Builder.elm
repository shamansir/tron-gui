module Tron.Builder exposing
    ( root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool
    , button, buttonWith, colorButton
    , nest, choice, choiceByCompare, strings, labels, palette
    , buttons, buttonsWithIcons, coloredButtons, setColor
    , Icon, addIcon, icon, iconAt, themedIcon, themedIconAt, makeUrl
    , toChoice, toSet, handleWith
    , expand, collapse
    , addPath, addLabeledPath, addLabels
    )


{-| The Builder helps you define the structure of your GUI. It is made as abstract from the representation as possible, so that any control could be changed to a better or alternative one without the requirement for you to change the definition. What you specify is the type of the value to be changed and the message to in the case of such change. Plus grouping, as deep as you want and as wide as the free space allows.

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

For `choice...` and `nest...` items you are required to specify both the shape of the panel (in cells) and shape of every cell (they only can be all of one shape).

Shape of the panel may be requested to be calculated automatically (`Shape.auto`) or asked to have the desired number of rows (`Shape.rows 3`) or columns (`Shape.columns 1`) and so auto-calculate the other side, or specified manually using `by` function (`Shape.by 4 3`).

Cell shapes are usually just 1x1 (`CellShape.single`), but in some specific cases you may want controls to be twice smaller (`CellShape.half`) or horizontal (`CellShape.oneByTwice`) or vertical (`CellShape.twiceByOne`). Some controls take completely another form when their cell shape is changed.

# The Naming

In the examples below, we use `Builder.` as a prefix in the places where we reference functions from this module. This assumes that you did something like this in your code:

    import Tron exposing (Tron)
    import Tron.Builder as Builder exposing (..)

However, it is ok to use any name you like, for sure. Be it `Tron.` or `Def.` or whatever...

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
        Builder.root
            [
                ( "amount"
                , Builder.float
                    { min = 0, max = 1, step = 0.01 }
                    amount
                    AmountChanged
                )
            ]

`Tron msg` is the type that represents any cell in your GUI. If it's a nesting, it also contains recursively other instances of `Tron msg`.

Use the methods in the module as the helpers in building your own grid structure:

    Builder.root
        [ ( "int", Builder.int ... )
        , ( "float", Builder.float ... )
        ,
            ( "nest"
            , Builder.nest
                [ ( "button", Builder.button ... )
                , ...
                ]
            )
        ]

Using `Tron msg` together with `Builder.map` you may build your GUI from several modules with different messages.

    Builder.root
        [ ( "one", ModuleOne.gui model.moduleOne |> Builder.map ModuleOne )
        , ( "two", ModuleTwo.gui model.moduleTwo |> Builder.map ModuleTwo )
        , ( "three", ModuleThree.gui model.moduleThree |> Builder.map ModuleThree )
        ]

For more information, see the `examples` folder in the source code.

# Sets

`Set msg` is just the list of components' definitions together with their labels. It is what
`Builder.root` and `Builder.nest` get as an argument. `Set msg` is exposed as a separate type to help you in the cases where you build your GUI from several modules, but want to join them in a single panel rather than nesting every module separately.

    Builder.nest
        Shape.auto -- we want the plate size to be calculate autimatically
        CellShape.single -- usual 1x1 shape of a cell
        <| (ModuleOne.gui |> List.map (Tuple.mapSecond <| Builder.map ToModuleOne))
            ++ (ModuleTwo.gui |> List.map (Tuple.mapSecond <| Builder.map ToModuleTwo))
            ++ (ModuleThree.gui |> List.map (Tuple.mapSecond <| Builder.map ToModuleThree))

# Root
@docs root

# Items
@docs none, int, float, number, xy, coord, color, text, input, button, buttonWith, colorButton, toggle, bool

# Groups
@docs nest, choice, choiceByCompare, strings, labels, palette

# Buttons
@docs buttons, buttonsWithIcons, coloredButtons, setColor

# Icons
@docs Icon, addIcon, icon, iconAt, themedIcon, themedIconAt, makeUrl

# Force expand / collapse for nesting
@docs expand, collapse

# Conversion
@docs toSet, toChoice, addLabels, handleWith

# Add Path
@docs addPath, addLabeledPath
-}


import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)
import Url.Builder as Url
import Dict

import Tron exposing (Tron)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (expand, collapse)
import Tron.Control exposing (Control(..))
import Tron.Util exposing (findMap)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape as Shape exposing (find, rows, cols)
import Tron.Style.Theme exposing (Theme)
import Tron.Pages exposing (PageNum)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url(..))
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool)
import Tron.Control.Nest as Nest exposing (Form(..), ItemId)

import Tron.Builder.Choice as Choice


{-|
See also: `Builder.Set`.
-}
type alias Tron msg = Property msg


{-| `Set msg` is just the list of components' definitions together with their labels. It is what
`Builder.root` and `Builder.nest` get as an argument. `Set msg` is exposed as a separate type to help you in the cases where you build your GUI from several modules, but want to join them in a single panel rather than nesting every module separately.

    Builder.nest
        Shape.auto -- we want the plate size to be calculate autimatically
        CellShape.single -- usual 1x1 shape of a cell
        <| (ModuleOne.gui |> List.map (Tuple.mapSecond <| Builder.map ToModuleOne))
            ++ (ModuleTwo.gui |> List.map (Tuple.mapSecond <| Builder.map ToModuleTwo))
            ++ (ModuleThree.gui |> List.map (Tuple.mapSecond <| Builder.map ToModuleThree))

See also: `Builder.map`.
-}
type alias Set msg = List ( Label, Tron msg )


{-| Similar to `Cmd.none`, `Sub.none` etc., makes it easier to use expressions in the definition.

For example:

    if user |> User.is Root then
        Builder.button RemoveAllFiles
    else
        Builder.none

-}
none : Tron msg
none = Nil


{-| Use the root only once, to mark the first visible row of your UI, and put anything else inside.

Actually it is just an alias for the nested row of controls, always expanded.

    for myModel =
        Builder.root
            [
                ( "octave"
                , Builder.int { min = 1, max = 8, step = 1 } myModel.octave ChangeOctave
                )
            ,
                ( "note"
                , Builder.int { min = 1, max = 127, step = 1 } myModel.midiNote ChangeMidiNote
                )
            ,
                ( "shape"
                , Builder.nest

                    [
                        ( "sine"
                        , Builder.buttonWith
                            (Builder.icon <| Url.Builder.relative [ "sawwave.svg" ] [])
                            (always <| ChangeShape Sine)
                        )
                    ,
                        ( "square"
                        , Builder.buttonWith
                            (Builder.icon <| Url.Builder.relative [ "squarewave.svg" ] [])
                            (always <| ChangeShape Square)
                        )
                    ,
                        ( "saw"
                        , Builder.buttonWith
                            (Builder.icon <| Url.Builder.relative [ "sawwave.svg" ] [])
                            (always <| ChangeShape Saw)
                        )
                    ]

                    <| always NoOp

                )
            ]

-}
root : Set msg -> Tron msg
root props =
    nest
        (Shape.rows 1)
        CS.single
        props
        |> expand


{-| `float` creates a control over a rational number value, with a minimum, maximum and a step.

    Builder.float { min = 0, max = 44000, step = 1 } myModel.frequency ChangeFrequency

-}
float : Axis -> Float -> ( Float -> msg ) -> Tron msg
float axis default =
    Number
        << Control axis default -- RoundBy 2
        << Just


{-| `int` creates a control over an integer number value, with a minimum, maximum and a step.

    Builder.int { min = 1, max = 8, step = 1 } myModel.octave ChangeOctave

-}
int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Tron msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


{-| `number` is the alias for `Builde.float`.

    Builder.number { min = 0, max = 44000, step = 1 } myModel.frequency ChangeFrequency
-}
number : Axis -> Float -> ( Float -> msg ) -> Tron msg
number = float


{-| `xy` creates a control over a pair of two number values or anything that can be translated to them.

    Builder.xy
        ( { min = 0, max = scene.width, step = 0.1 }
        , { min = 0, max = scene.height, step = 0.1 }
        )
        myModel.lightDirection
        PointLightTo

-}
xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Tron msg
xy axes default =
    Coordinate
        << Control axes default
        << Just


{-| `coord` is the alias for `Builder.xy`
-}
coord : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Tron msg
coord = xy


{-| `input` creates a control over a value which can be translated to `String` and parsed from `String`. It is just a helper over `text` control.

    Builder.input
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
input toString fromString default toMsg =
    Text
        <| Control
            ()
            ( Ready, toString default )
            (Just <| Tuple.second >> fromString >> Maybe.withDefault default >> toMsg)


{-| `text` creates a control over a `String` value.

    Builder.text model.elfName RenameElf
-}
text : String -> (String -> msg) -> Tron msg
text default handler =
    Text
        <| Control
            ()
            ( Ready, default )
            (Just <| Tuple.second >> handler)


{-| `color` creates a control over a color, for the moment it is Hue/Saturation in 2D space, same as `xy`, but with different representation, but we may improve it later. Or you may change it to `choice` with your own palette.

The `Color` type here is from `avh4/elm-color` module.

    Builder.color model.lightColor AdjustColor
-}
color : Color -> (Color -> msg) -> Tron msg
color default =
    Color
        << Control
            ()
            default
        << Just


{-| `button` creates a control over a _unit_ `()` value. Type science aside, when you receive the unit value `()` in the handler, it just means that this button was pushed.

    Builder.button <| always DoABang
-}
button : (() -> msg) -> Tron msg
button =
    buttonByFace Default


{-| -}
type alias Icon = Button.Icon


{-| Create an `Icon` from its URL or filename.

    import Url.Builder as Url

    Builder.icon
        <| makeUrl <| Url.relative [ "assets", "myicon.svg" ] []

See also: `Builder.iconAt`, `Builder.themedIcon`, `Builder.themedIconAt`
-}
icon : Url -> Icon
icon = Button.icon


{-| Create an `Icon` using its relative local path.

    Builder.iconAt [ "assets", "myicon.svg" ]

See also: `Builder.themedIconAt`
-}
iconAt : List String -> Icon
iconAt = Button.iconAt


{-| Create a themed `Icon` from its URL or filename.

    import Url.Builder as Url

    Builder.themedIcon
        <| \theme ->
            makeUrl <| Url.relative [ "assets", "myicon_" ++ Theme.toString theme ++ ".svg" ] []
-}
themedIcon : (Theme -> Url) -> Icon
themedIcon = Button.themedIcon


{-| Create a themed `Icon` using its relative local path.

    Builder.themedIconAt
        <| \theme -> [ "assets", "myicon_" ++ Theme.toString theme ++ ".svg" ]
-}
themedIconAt : (Theme -> List String) -> Icon
themedIconAt = Button.themedIconAt


{-| Make URL from String
-}
makeUrl : String -> Url
makeUrl = Button.makeUrl


{-| Same as `button`, but with icon instead of a boring square. See `icon` function as a helper to define icon using its URL. SVG files preferred, keep in mind that you'll need to host them somewhere nearby, for example using simple HTTP server.

    Builder.button (Builder.iconAt [ "red-button.svg" ]) <| always DoABang

Or:

    Builder.button
        (Builder.themedIconAt
            <| \theme -> [ "my-icon-" ++ Theme.toString theme ++ ".svg" ]
        )
        <| always DoABang

See also: `Builder.icon`,  `Builder.iconAt`, `Builder.themedIcon`,  `Builder.themedIconAt`
-}
buttonWith : Icon -> (() -> msg) -> Tron msg
buttonWith icon_ =
    buttonByFace <| WithIcon icon_



{-| Same as `button`, but having a color as it face.

    Builder.button (Builder.icon "red-button.svg") <| always DoABang
-}
colorButton : Color -> (() -> msg) -> Tron msg
colorButton color_ =
    buttonByFace <| WithColor color_


-- not exposed
buttonByFace : Face -> (() -> msg) -> Tron msg
buttonByFace face =
    Action
        << Control
            face
            ()
        << Just


{-| `toggle` creates a control over a boolean value.

    Builder.toggle model.lightOn SwitchLight
-}
toggle : Bool -> (Bool -> msg) -> Tron msg
toggle default toMsg =
    Toggle
        <| Control
            ()
            (boolToToggle default)
            (Just <| toggleToBool >> toMsg)


{-| `bool` is the alias for `Builder.toggle`
-}
bool : Bool -> (Bool -> msg) -> Tron msg
bool = toggle


{-| `nest` lets you group other controls (including other `nest`ings) under a button which expands a group. Also, this group can be _detached_ if GUI is confugured so.

Handler receives the state of the group, like if it is exapanded or collapsed or detached, but usually it's fine just to make it `always NoOp`.

    Builder.nest
        ( cols 1 ) -- we want the plate to have one column
        CellShape.single -- usual 1x1 shape of a cell
        [
            ( "red"
            , Builder.float { min = 0, max = 255, step = 0.1 } model.red <| AdjustColor Red
            )
        ,
            ( "green"
            , Builder.float { min = 0, max = 255, step = 0.1 } model.blue <| AdjustColor Green
            )
        ,
            ( "blue"
            , Builder.float { min = 0, max = 255, step = 0.1 } model.blue <| AdjustColor Blue
            )
        ]

See also: `Style.Shape`, `Style.CellShape`
-}
nest : PanelShape -> CellShape -> Set msg -> Tron msg
nest panelShape cellShape items =
    Group
        Nothing
        ( panelShape
        , cellShape
        )
        <| Control
            ( Array.fromList items
            )
            { form = Collapsed
            , face = Nothing
            , page = 0
            }
            Nothing


{-| Set icon as face to the button _or_ the button of a `nest` control.
-}
addIcon : Icon -> Tron msg -> Tron msg
addIcon icon_ =
    Property.setFace <| WithIcon icon_


{-| Set color as face to the button _or_ the button of a `nest` control.
-}
setColor : Color -> Tron msg -> Tron msg
setColor color_ =
    Property.setFace <| WithColor color_


{-| `choice` defines a list of options for user to choose between. Consider it as `<select>` tag with `<option>`s. When some option is chosen by user, the handler gets the corresponding value. Notice that we ask for `comparable` type here.

    Builder.choice
        ( 5, 4 ) -- the wanted shape of the controls, i.e. 5 cells width x 4 cells height
        CellShape.single -- usual 1x1 shape of a cell
        ([ 128, 256, 512 ]
            |> buttons
            |> addLabels String.fromInt
        )
        model.bitrate
        ChangeBitrate

*NB*: If you don't want to use `comparable` types, but rather want to specify you own compare function, use `choiceByCompare`.

*NB*: If you want to add icons to the buttons, use `Builder.buttonsWithIcons`, for colors use `Builder.coloredButtons`.

See also: `Builder.choiceByCompare`, `Builder.strings`, `Builder.palette`, `Style.Shape`, `Style.CellShape`
-}
choice
     : PanelShape
    -> CellShape
    -> Set comparable
    -> comparable
    -> ( comparable -> msg )
    -> Tron msg
choice shape cellShape set current toMsg =
    Choice.helper
        ( shape, cellShape )
        set
        current
        (==)
        (Tuple.second >> toMsg)


{-| `choiceByCompare` is identical to `choice`, but asks user for a custom comparison function instead of requiring `comparable` values.

    Builder.choiceByCompare
        ( cols 1 ) -- we want the plate to have one column
        CellShape.single -- usual 1x1 shape of a cell
        ([ Sine, Square, Triangle, Saw ]
            |> buttons
            |> addLabels waveToString
        )
        model.waveShape
        compareWaves -- sometimes just (==) works, but it's better not to rely on it
        ChangeWaveShape

See also: `Builder.strings`, `Builder.palette`, `Style.Shape`, `Style.CellShape`
-}
choiceByCompare
     : PanelShape
    -> CellShape
    -> Set a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Tron msg
choiceByCompare shape cellShape set current compare toMsg =
    Choice.helper
        ( shape, cellShape )
        set
        current
        compare
        (Tuple.second >> toMsg)


{-| `strings` is a helper to create `choice` over string values.

    Builder.strings
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
        (cols 1)
        CS.twiceByHalf
        (options
            |> buttons
            |> addLabels identity
        )
        current
        toMsg


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

        optionsAsLabels =
            options
            |> buttons
            |> addLabels toLabel

    in choice
        (cols 1)
        CS.twiceByHalf
        (options
            |> List.map toLabel
            |> buttons
            |> addLabels identity
        )
        (toLabel current)
        (\label ->
            labelToValue
                |> Dict.get label
                |> Maybe.map toMsg
                |> Maybe.withDefault fallback
        )


{-| `palette` is a helper to create `choice` over color values.

    Builder.palette
        [ Color.aqua, Color.rouge, Color.vanilla ]
        model.iceCreamColor
        RepaintIceCream
-}
palette
     : PanelShape
    -> List Color
    -> Color
    -> (Color -> msg)
    -> Tron msg
palette panelShape options current =
    choiceByCompare
        panelShape
        CS.half
        (options
            |> coloredButtons identity
            |> addLabels Color.colorToHexWithAlpha
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


{-| `buttons` is the helper to translate a list of anything to a list of buttons.

It could be useful to pass such list to `choice` or `nest`:

    Builder.choiceByCompare
        single
        (cols 2)
        ([ Sine, Square, Triangle, Saw ]
            |> buttons
            |> addLabels waveToString
        )
        model.waveShape
        compareWaves -- sometimes just (==) works, but it's better not to rely on it
        ChangeWaveShape

Or:

    Builder.nest
        single
        (cols 2)
        ([ Sine, Square, Triangle, Saw ]
            |> buttons
            |> toSet waveToString -- `toSet` is just another name for `addLabels`
            |> handleWith ChangeWaveShape
        )

-}
buttons : List a -> List (Tron a)
buttons =
    List.map (button << always)


{-| `buttonsWith` is the helper to translate a list of anything to a list of buttons with icons:

    Builder.choiceByCompare
        single
        (cols 2)
        ([ Sine, Square, Triangle, Saw ]
            |> buttonsWith
                (\wave ->
                    Builder.themedIconAt
                        <| \theme -> [ "assets", Theme.toString theme, waveToString wave ++ ".svg" ]
                )
            |> addLabels waveToString
        )
        model.waveShape
        compareWaves -- sometimes just (==) works, but it's better not to rely on it
        ChangeWaveShape

See also: `Builder.buttons`, `Builder.icon`, `Builder.themedIcon`, `Builder.handleWith`
-}
buttonsWithIcons : (a -> Icon) -> List a -> List (Tron a)
buttonsWithIcons toIcon =
    List.map (\v -> buttonWith (toIcon v) <| always v)


{-| `coloredButtons` is the helper to translate a list of anything to a list of buttons with colors:

    Builder.choiceByCompare
        single
        (cols 2)
        ([ Color.aqua, Color.rouge, Color.vanilla ]
            |> buttonsWith
                (\wave ->
                    Builder.themedIconAt
                        <| \theme -> [ "assets", Theme.toString theme, waveToString wave ++ ".svg" ]
                )
            |> addLabels waveToString
        )
        model.iceCreamColor
        compareColors -- sometimes just (==) works, but it's better not to rely on it
        ce

See also: `Builder.palette`, `Builder.buttons`, `Builder.handleWith`
-}
coloredButtons : (a -> Color) -> List a -> List (Tron a)
coloredButtons toColor =
    List.map (\v -> colorButton (toColor v) <| always v)


{-| Convert a list of components to a set by adding labels.
-}
toSet : (a -> Label) -> List (Tron a) -> Set a
toSet toLabel =
    List.map
        (\prop ->
            Tron.Property.evaluate__ prop
                |> Maybe.map
                    (\v ->
                        ( toLabel v
                        , prop
                        )
                    )
        )
    >> List.filterMap identity


{-| Convert a list of components to a set by adding labels.

Same as `Builder.toSet`
-}
addLabels : (a -> Label) -> List (Tron a) -> Set a
addLabels =
    toSet


{-| convert a `Set` of controls holding any values to
a `Set` of messages, technically would be the same as `Set.map`
-}
handleWith : (a -> msg) -> Set a -> Set msg
handleWith f = List.map (Tuple.mapSecond <| Tron.map f)


{-| Forcefully expand the nesting:

    Builder.nest ... |> Builder.expand
    Builder.choice ... |> Builder.expand
-}
expand : Tron msg -> Tron msg
expand = Property.expand


{-| Forcefully collapse the nesting:

    Builder.nest ... |> Builder.collapse
    Builder.choice ... |> Builder.collapse
-}
collapse : Tron msg -> Tron msg
collapse = Property.collapse


{- Set the icon to the control that can have it:

    Builder.nest ... |> Builder.setIcon (Builder.icon ...)
    Builder.button ... |> Builder.setIcon (Builder.icon ...)
-}
-- TODO
-- setIcon : Icon -> Tron msg -> Tron msg
-- setIcon icon = Property.collapse


{-| Add the path representing the label-based way to reach the
particular control in the GUI tree.
-}
addPath : Tron msg -> Tron ( List Int, msg )
addPath = Property.addPath >> Tron.map (Tuple.mapFirst Path.toList)


{-| Add the path representing the label-based way to reach the
particular control in the GUI tree.
-}
addLabeledPath : Tron msg -> Tron ( List String, msg )
addLabeledPath = Property.addLabeledPath


{-| Convert a `nest` control to `choice` control. It can be done
easily by specifying a handler:

    Builder.nest
        single
        (cols 2)
        ([ Sine, Square, Triangle, Saw ]
            |> buttons
            |> toSet waveToString -- `toSet` is just another name for `addLabels`
            |> handleWith ChangeWaveShape
        )
    |> toChoice ChangeShapeById
-}
toChoice : (ItemId -> msg) -> Tron msg -> Tron msg
toChoice = Property.toChoice
