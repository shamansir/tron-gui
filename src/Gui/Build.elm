module Gui.Build exposing (..)


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

# Root
@docs root

# Items
@docs none, int, float, xy, text, input, color, button, button1, toggle

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


type alias Builder msg = Property msg


{-| Similar to `Cmd.none`, `Sub.none` etc., makes it easier to use expressions in the definition.

For example:

    if user |> User.is Root then
        Gui.button RemoveAllFiles
    else
        Gui.none
-}
none : Builder msg
none = Nil


{-| Use the root only once, to mark the first visible row of your UI, and put anything else inside.

Actually it is just an alias for the nested row of controls, always expanded.
-}
root : List (Label, Builder msg) -> (GroupState -> msg) -> Builder msg
root props =
    nestIn ( 1, List.length <| List.filter (Tuple.second >> isGhost >> not) props ) props
        >> expand


{-| `float` creates a control over a rational number value, with a minimum, maximum and a step.
-}
float : Axis -> Float -> ( Float -> msg ) -> Builder msg
float axis default =
    Number
        << Control axis default -- RoundBy 2


{-| `int` creates a control over an integer number value, with a minimum, maximum and a step.
-}
int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Builder msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


{-| `xy` creates a control over a pair of two number values or anything that can be translated to them.
-}
xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Builder msg
xy axes default =
    Coordinate
        << Control axes default


{-| `input` creates a control over a value which can be translated to `String` and parsed from `String`. It is just a helper over `text` control.
-}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Builder msg
input toString fromString default toMsg =
    Text
        <| Control
            ()
            ( Ready, toString default)
            (Tuple.second >> fromString >> Maybe.withDefault default >> toMsg)


{-| `text` creates a control over a `String` value,
-}
text : String -> (String -> msg) -> Builder msg
text default handler =
    Text
        <| Control
            ()
            (Ready, default)
            (Tuple.second >> handler)


{-| `color` creates a control over a color, for the moment it is Hue/Saturation in 2D space, same as `xy`, but with different representation, but we may improve it later. Or you may change it to `choice` with your own palette.
-}
color : Color -> (Color -> msg) -> Builder msg
color default =
    Color
        << Control
            ()
            default


{-| `button` creates a control over a _unit_ `()` value. Type science aside, when you receive the unit value `()` in the handler, it just means that button was pushed.
-}
button : (() -> msg) -> Builder msg
button =
    Action
        << Control
            Nothing
            ()


{-| Create an `Icon` from its URL or filename.
-}
icon : String -> Icon
icon = Icon


{-| Same as `button`, but with icon instead of a boring square. See `icon` function as a helper to define icon using its URL. SVG files preferred, keep in mind that you'll need to host them somewhere nearby, for example using simple HTTP server.
-}
button1 : Icon -> (() -> msg) -> Builder msg
button1 icon_ =
    Action
        << Control
            (Just icon_)
            ()


{-| `toggle` creates a control over a boolean value. But to make it friendlier, there's a `ToggleState` type to represent it.
-}
toggle : ToggleState -> (ToggleState -> msg) -> Builder msg
toggle default =
    Toggle
        << Control
            ()
            default


-- FIXME: get rid of the handler having almost no sense
{-| `nest` lets you group other controls (including other `nest`ings) under a button which expands a group. Also, this group can be _detached_ if GUI is confugured so.

Handler receives the state of the group, like if it is exapanded or collapsed or detached, but usually it's fine just to make it `always NoOp`.
-}
nest : List (Label, Builder msg) -> (GroupState -> msg) -> Builder msg
nest items =
    nestIn (findShape items) items


-- FIXME: get rid of the handler having almost no sense
{-| `nestIn` is the same as `nets`, but lets you define the shape in which items will be grouped, instead of automatic calculations.
-}
nestIn : Shape -> List (Label, Builder msg) -> (GroupState -> msg) -> Builder msg
nestIn shape items handler =
    Group
        <| Control
            ( shape
            , Array.fromList items
            )
            ( Collapsed
            , Nothing
            )
            (Tuple.first >> handler)


{-| `choice` defines a list of options for user to choose between. Consider it as `<select>` tag with `<option>`s. When some option is chosen by user, the handler gets the corresponding value. Thanks to Elm rich type system, you are not limited to strings, the option can have any type. But since we also translate these values to HTML and JSON, you need to specify the converter to `String` and from it. Also, since we don't ask for `comparable` type here, you are asked to provide comparison function.
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
                (Tuple.second >> Tuple.second >> callByIndex)


{-| `strings` is a helper to create `choice` over string values.
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
