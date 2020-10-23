module Gui.Build exposing
    ( Builder
    , root
    , none, int, float, xy, color, text, input, button, button1, toggle, toggle1
    , nest, nestIn, choice, choice1, choiceIn, strings
    , icon
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


# Builder
@docs Builder

# Root
@docs root

# Items
@docs none, int, float, xy, color, text, input, button, button1, toggle, toggle1

# Groups
@docs nest, nestIn, choice, choice1, choiceIn, strings

# Icons
@docs icon

-}

import Array
import Color exposing (Color)

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Control exposing (Control(..))
import Gui.Util exposing (findMap)


{-| `Builder msg` is the type that represents any cell in your GUI. If it's a nesting, it also contains recursively other instance `Builder msg`.

Use the method in the module as the helpers in building your own grid structure.

When it's done, use `Gui.init` to create the user interface from your description:

    Gui.init <|
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
-}
type alias Builder msg = Property msg


{-| Similar to `Cmd.none`, `Sub.none` etc., makes it easier to use expressions in the definition.

For example:

    if user |> User.is Root then
        Builder.button RemoveAllFiles
    else
        Builder.none

-}
none : Builder msg
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
                        , Builder.button1
                            (Builder.icon "sineewave.svg")
                            (always <| ChangeShape Sine)
                        )
                    ,
                        ( "square"
                        , Builder.button1
                            (Builder.icon "squarewave.svg")
                            (always <| ChangeShape Square)
                        )
                    ,
                        ( "saw"
                        , Builder.button1
                            (Builder.icon "sawwave.svg")
                            (always <| ChangeShape Saw)
                        )
                    ]

                    <| always NoOp

                )
            ]

-}
root : List (Label, Builder msg) -> Builder msg
root props =
    nestIn ( 1, List.length <| List.filter (Tuple.second >> isGhost >> not) props ) props
        |> expand


{-| `float` creates a control over a rational number value, with a minimum, maximum and a step.

    Builder.float { min = 0, max = 44000, step = 1 } myModel.frequency ChangeFrequency

-}
float : Axis -> Float -> ( Float -> msg ) -> Builder msg
float axis default =
    Number
        << Control axis default -- RoundBy 2
        << Just


{-| `int` creates a control over an integer number value, with a minimum, maximum and a step.

    Builder.int { min = 1, max = 8, step = 1 } myModel.octave ChangeOctave

-}
int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Builder msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


{-| `xy` creates a control over a pair of two number values or anything that can be translated to them.

    Builder.xy
        ( { min = 0, max = scene.width, step = 0.1 }
        , { min = 0, max = scene.height, step = 0.1 }
        )
        myModel.lightDirection
        PointLightTo

-}
xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Builder msg
xy axes default =
    Coordinate
        << Control axes default
        << Just


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
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Builder msg
input toString fromString default toMsg =
    Text
        <| Control
            ()
            ( Ready, toString default)
            (Just <| Tuple.second >> fromString >> Maybe.withDefault default >> toMsg)


{-| `text` creates a control over a `String` value.

    Builder.text model.elfName RenameElf
-}
text : String -> (String -> msg) -> Builder msg
text default handler =
    Text
        <| Control
            ()
            (Ready, default)
            (Just <| Tuple.second >> handler)


{-| `color` creates a control over a color, for the moment it is Hue/Saturation in 2D space, same as `xy`, but with different representation, but we may improve it later. Or you may change it to `choice` with your own palette.

The `Color` type here is from `avh4/elm-color` module.

    Builder.color model.lightColor AdjustColor
-}
color : Color -> (Color -> msg) -> Builder msg
color default =
    Color
        << Control
            ()
            default
        << Just


{-| `button` creates a control over a _unit_ `()` value. Type science aside, when you receive the unit value `()` in the handler, it just means that this button was pushed.

    Builder.button <| always DoABang
-}
button : (() -> msg) -> Builder msg
button =
    Action
        << Control
            Nothing
            ()
        << Just


{-| Create an `Icon` from its URL or filename.

    Builder.icon "assets/myicon.svg"
-}
icon : String -> Icon
icon = Icon


{-| Same as `button`, but with icon instead of a boring square. See `icon` function as a helper to define icon using its URL. SVG files preferred, keep in mind that you'll need to host them somewhere nearby, for example using simple HTTP server.

    Builder.button (Builder.icon "red-button.svg") <| always DoABang
-}
button1 : Icon -> (() -> msg) -> Builder msg
button1 icon_ =
    Action
        << Control
            (Just icon_)
            ()
        << Just


{-| `toggle` creates a control over a boolean value.

    Builder.toggle model.lightOn SwitchLight
-}
toggle : Bool -> (Bool -> msg) -> Builder msg
toggle default toMsg =
    Toggle
        <| Control
            ()
            (boolToToggle default)
            (Just <| toggleToBool >> toMsg)


{-| `toggle1` creates a control over a boolean value. But to make it friendlier, there's a `ToggleState` type to represent it.

    Builder.toggle1
        (case model.lightOn of
            LightOn -> ToggleOn
            LightOff -> ToggleOff
        )
        (\nextState ->
            case nextState of
                ToggleOn -> Switch LightOn
                ToggleOff -> Switch LightOff
        )
-}
toggle1 : ToggleState -> (ToggleState -> msg) -> Builder msg
toggle1 default =
    Toggle
        << Control
            ()
            default
        << Just


{-| `nest` lets you group other controls (including other `nest`ings) under a button which expands a group. Also, this group can be _detached_ if GUI is confugured so.

Handler receives the state of the group, like if it is exapanded or collapsed or detached, but usually it's fine just to make it `always NoOp`.

    Builder.nest

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
-}
nest : List (Label, Builder msg) -> Builder msg
nest items =
    nestIn (findShape items) items


{-| `nestIn` is the same as `nest`, but lets you define the shape in which items will be grouped, instead of automatic calculations.

    Builder.nest
        (5, 5)
        [ ... ]
        <| always NoOp
-}
nestIn : Shape -> List (Label, Builder msg) -> Builder msg
nestIn shape items =
    Group
        <| Control
            ( shape
            , Array.fromList items
            )
            ( Collapsed
            , Nothing
            )
            Nothing -- (Tuple.first >> handler)


{-| `choice` defines a list of options for user to choose between. Consider it as `<select>` tag with `<option>`s. When some option is chosen by user, the handler gets the corresponding value. Thanks to Elm rich type system, you are not limited to strings, the option can have any type. But since we also translate these values to HTML and JSON, you need to specify the converter to `String` and from it. Also, since we don't ask for `comparable` type here, you are asked to provide comparison function.

    Builder.choice
        (\waveShape ->
            case waveShape of
                Sine -> "sine"
                Square -> "square"
                Triangle -> "triangle"
                Saw -> "saw"
        )
        [ Sine, Square, Triangle, Saw ]
        model.waveShape
        (==) -- equality operator usually works for sum types, but be accurate
        ChangeWaveShape
-}
choice
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Builder msg
choice f items =
    choiceIn (findShape items) f items


{-| `choice1` is the same as `choice`, but works with `comparable` values.

    Builder.choice1
        String.fromInteger
        [ 128, 256, 512 ]
        model.bitrate
        (String.toInteger >> Maybe.withDefault 128 >> ChangeBitrate)
-}
choice1
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> ( comparable -> msg )
    -> Builder msg
choice1 f items v =
    choiceIn (findShape items) f items v (==)


{-| `choiceIn` is the same as `choice`, but lets you define the shape in which items will be grouped, instead of automatic calculations.

    Builder.choiceIn
        ( 5, 4 )
        [ ... ]
        ...
-}
choiceIn
     : Shape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Builder msg
choiceIn shape toLabel options current compare toMsg =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex (Selected indexToCall) =
            indexedOptions
                |> findMap
                    (\(index, option) ->
                        if indexToCall == index
                            then Just option
                            else Nothing
                    )
                |> Maybe.map toMsg
                |> Maybe.withDefault (toMsg current)
    in
        Choice
            <| Control
                ( shape
                , options
                    |> List.map toLabel
                    |> List.indexedMap
                        (\index label ->
                            ( label
                            , button <| always <| callByIndex <| Selected index
                            )
                        )
                    |> Array.fromList
                )
                ( Collapsed
                ,
                    ( Nothing
                    , indexedOptions
                        |> findMap
                            (\(index, option) ->
                                if compare option current
                                    then Just index
                                    else Nothing
                            )
                        |> Maybe.withDefault 0
                        |> Selected
                    )
                )
                (Just <| Tuple.second >> Tuple.second >> callByIndex)


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
    -> Builder msg
strings options current toMsg =
    choice
        identity
        options
        current
        ((==))
        toMsg


findShape : List a -> ( Int, Int )
findShape items =
    ( 1, List.length items )
