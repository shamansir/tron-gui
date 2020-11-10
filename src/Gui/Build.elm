module Gui.Build exposing
    ( Builder
    , root
    , none, int, float, xy, color, text, input, button, buttonWith, toggle
    , nest, choice, choiceAuto, choiceIcons, strings, palette
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

# Shapes

For `choice...` and `nest...` items you are required to specify both shape of the panel (in cells) and shape of every cell (they only can be all of one shape). Shape of the panel depends on the shape of its inner cells. If it's `Full` (default), no worries. Else, if it's `TwiceByHalf` (2.0 x 0.5), for example, and the panel shape is still specified in full cells, so to place six such items in one column, you'll need to give panel a shape of 2 x 3: 1*2.0 x 6*0.5.

# Builder
@docs Builder

# Root
@docs root

# Items
@docs none, int, float, xy, color, text, input, button, buttonWith, toggle

# Groups
@docs nest, choice, choiceIcons, choiceAuto, strings, palette

# Icons
@docs icon

-}

import Array
import Color exposing (Color)

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Control exposing (Control(..))
import Gui.Util exposing (findMap)
import Gui.Render.Style exposing (CellShape(..))


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
                        , Builder.buttonWith
                            (Builder.icon "sineewave.svg")
                            (always <| ChangeShape Sine)
                        )
                    ,
                        ( "square"
                        , Builder.buttonWith
                            (Builder.icon "squarewave.svg")
                            (always <| ChangeShape Square)
                        )
                    ,
                        ( "saw"
                        , Builder.buttonWith
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
    nest
        ( 1, props
                |> List.filter (Tuple.second >> isGhost >> not)
                |> List.length )
        Full
        props
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
    buttonByFace Default


{-| Create an `Icon` from its URL or filename.

    Builder.icon "assets/myicon.svg"
-}
icon : String -> Icon
icon = Icon


{-| Same as `button`, but with icon instead of a boring square. See `icon` function as a helper to define icon using its URL. SVG files preferred, keep in mind that you'll need to host them somewhere nearby, for example using simple HTTP server.

    Builder.button (Builder.icon "red-button.svg") <| always DoABang
-}
buttonWith : Icon -> (() -> msg) -> Builder msg
buttonWith icon_ =
    buttonByFace <| WithIcon icon_


-- not exposed
buttonByFace : Face -> (() -> msg) -> Builder msg
buttonByFace face =
    Action
        << Control
            face
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


{-| `nest` lets you group other controls (including other `nest`ings) under a button which expands a group. Also, this group can be _detached_ if GUI is confugured so.

Handler receives the state of the group, like if it is exapanded or collapsed or detached, but usually it's fine just to make it `always NoOp`.

    Builder.nest
        ( 5, 4 ) -- the wanted shape of the nesting, i.e. 5 cells width x 4 cells height
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
nest : Shape -> CellShape -> List (Label, Builder msg) -> Builder msg
nest shape cellShape items =
    Group
        <| Control
            ( ( shape, cellShape )
            , Array.fromList items
            )
            ( Collapsed
            , Nothing
            )
            Nothing -- (Tuple.first >> handler)


{-| `choice` defines a list of options for user to choose between. Consider it as `<select>` tag with `<option>`s. When some option is chosen by user, the handler gets the corresponding value. Thanks to Elm rich type system, you are not limited to strings, the option can have any type. But since we also translate these values to HTML and JSON, you need to specify the converter to `String` and from it. Also, since we don't ask for `comparable` type here, you are asked to provide a comparison function.

    Builder.choice
        ( 5, 4 ) -- the wanted shape of the nesting, i.e. 5 cells width x 4 cells height
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

See also: `Builder.strings`, `Builder.palette`
-}
choice
     : Shape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Builder msg
choice shape cellShape toLabel =
    choiceHelper
        ( shape, cellShape )
        (\callByIndex index val ->
            ( toLabel val
            , button <| always <| callByIndex <| SelectedAt index
            )
        )


{-| `choiceIcons` is the same as `choice`, but allows user to define icons for buttons.

    Builder.choice2
        (\waveShape ->
            case waveShape of
                Sine -> ( "sine", icon "sine.svg" )
                Square -> ( "square", icon "square.svg" )
                Triangle -> ( "triangle", icon "triangle.svg" )
                Saw -> ( "saw", icon "saw.svg" )
        )
        [ Sine, Square, Triangle, Saw ]
        model.waveShape
        (==) -- equality operator usually works for sum types, but be accurate
        ChangeWaveShape
-}
choiceIcons
    : Shape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Builder msg
choiceIcons shape cellShape toLabelAndIcon =
    choiceHelper
        ( shape, cellShape )
        (\callByIndex index val ->
            let ( label, theIcon ) = toLabelAndIcon val
            in
                ( label
                , buttonWith theIcon <| always <| callByIndex <| SelectedAt index
                )
        )


{-| `choiceAuto` is the same as `choice`, but works with `comparable` values.

    Builder.choiceAuto
        ( 5, 4 ) -- the wanted shape of the controls, i.e. 5 cells width x 4 cells height
        String.fromInteger
        [ 128, 256, 512 ]
        model.bitrate
        (String.toInteger >> Maybe.withDefault 128 >> ChangeBitrate)
-}
choiceAuto
     : Shape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> ( comparable -> msg )
    -> Builder msg
choiceAuto shape cellShape f items v =
    choice shape cellShape f items v (==)


choiceHelper
     : ( Shape, CellShape )
    -> ( (SelectedAt -> msg) -> Int -> a -> ( Label, Builder msg ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Builder msg
choiceHelper shape toBuilder options current compare toMsg =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex (SelectedAt indexToCall) =
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
                    |> List.indexedMap (toBuilder callByIndex)
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
                        |> SelectedAt
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
        ( 2
        , if (modBy 2 <| List.length options) == 0
            then List.length options // 2
            else List.length options // 2 + 1
        )
        TwiceByHalf
        identity
        options
        current
        ((==))
        toMsg


{-| `palette` is a helper to create `choice` over color values.

    Builder.palette
        greekChildrenNames
        model.currentChildName
        ChangeChildName
-}
palette
     : Shape
    -> List Color
    -> Color
    -> (Color -> msg)
    -> Builder msg
palette shape options current =
    choiceHelper
        ( shape, Half )
        (\callByIndex index val ->
            ( Color.toCssString val
            , buttonByFace (WithColor val) <| always <| callByIndex <| SelectedAt index
            )
        )
        options
        current
        (\cv1 cv2 ->
            case ( cv1 |> Color.toRgba, cv2 |> Color.toRgba ) of
                ( c1, c2 ) ->
                    (c1.red == c2.red) &&
                    (c1.blue == c2.blue) &&
                    (c1.green == c2.green) &&
                    (c1.alpha == c2.alpha)
        )


findShape : List a -> ( Int, Int )
findShape items =
    ( 1, List.length items )
