module Tron.Property.Build exposing (..)


import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)

import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property
import Tron.Property.Controls as Property
import Tron.Control exposing (Control(..))
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape exposing (rows, cols)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Button as Button exposing (Face(..), Icon(..), Url(..))
import Tron.Control.Impl.Toggle exposing (boolToToggle)
import Tron.Control.Impl.Nest exposing (Form(..))

import Tron.Property.Build.Choice as Choice


type alias Set a = List (Path.Label, Property a)


mapSet : (a -> b) -> Set a -> Set b
mapSet =
    List.map << Tuple.mapSecond << Property.map


none : a -> Property a
none = Nil


root : Set a -> a -> Property a
root props a =
    nest
        props
        a
        |> expand
        |> shape (rows 1)


float : Axis -> Float -> a -> Property a
float axis value a =
    Number
        <| Control axis ( Nothing, value )
        <| a



int : { min: Int, max : Int, step : Int } -> Int -> a -> Property a
int { min, max, step } default a =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        a


number : Axis -> Float -> a -> Property a
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> a -> Property a
xy axes value a =
    Coordinate
        <| Control axes ( Nothing, value )
        <| a


coord : ( Axis, Axis ) -> ( Float, Float ) -> a -> Property a
coord = xy


input : ( x -> String ) -> x -> a -> Property a
input toString value a = -- FIXME: accept just `String` and `value`
    Text
        <| Control
            ()
            ( Ready, toString value )
        <| a


text : String -> a -> Property a
text value a =
    Text
        <| Control
            ()
            ( Ready, value )
        <| a


color : Color -> a -> Property a
color value a =
    Color
        <| Control
            ()
            ( Nothing, value )
        <| a


button : a -> Property a
button =
    buttonByFace Default


face : Face -> Property a -> Property a
face =
    Property.setFace


icon : Url -> Face
icon = Button.icon >> WithIcon


iconAt : List String -> Face
iconAt = Button.iconAt >> WithIcon


themedIcon : (Theme -> Url) -> Face
themedIcon = Button.themedIcon >> WithIcon


themedIconAt : (Theme -> List String) -> Face
themedIconAt = Button.themedIconAt >> WithIcon


makeUrl : String -> Url
makeUrl = Button.makeUrl


-- not exposed
buttonByFace : Face -> a -> Property a
buttonByFace face_ a =
    Action
        <| Control
            face_
            ()
        <| a


toggle : Bool -> a -> Property a
toggle value a =
    Toggle
        <| Control
            ()
            (boolToToggle value)
        <| a


bool : Bool -> a -> Property a
bool = toggle


nest : Set a -> a -> Property a
nest items a =
    Group
        Nothing
        Property.defaultNestShape
        <| Control
            ( Array.fromList items
            )
            { form = Collapsed
            , face = Nothing
            , page = 0
            }
            <| a


useColor : Color -> Face
useColor = WithColor


choice
     : Set comparable
    -> comparable
    -> Property (Int, comparable)
choice set current =
    Choice.helper
        Property.defaultNestShape
        set
        current
        (==)


choiceBy
     : Set a
    -> a
    -> ( a -> a -> Bool )
    -> Property (Int, a)
choiceBy set current compare =
    Choice.helper
        Property.defaultNestShape
        set
        current
        compare


strings
     : List String
    -> String
    -> Property (Int, String)
strings options current =
    choice
        (options
            |> buttons
            |> addLabels identity
        )
        current
    |> shape (cols 1)
    |> cells CS.twiceByHalf


labels
     : ( a -> Path.Label )
    -> List a
    -> a
    -> Property ( Int, Path.Label )
labels toLabel options current =
    {- let
        labelToValue =
            options
                |> List.map (\v -> ( toLabel v, v ) )
                |> Dict.fromList
    in -}
    choice
        (options
            |> List.map toLabel
            |> buttons
            |> addLabels identity
        )
        (toLabel current)
        |> shape (cols 1)
        |> cells CS.twiceByHalf


palette
     : List ( Path.Label, Color )
    -> Color
    -> Property ( Int, Color )
palette options current =
    choiceBy
        (options
            |> buttons
            |> List.map (Property.with (face << useColor << Tuple.second))
            |> addLabels Tuple.first
            |> mapSet Tuple.second
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
    |> cells CS.half


buttons : List a -> List (Property a)
buttons =
    List.map button


toSet : (a -> Path.Label) -> List (Property a) -> Set a
toSet toLabel =
    List.map
        (\prop ->
            ( toLabel <| Tron.Property.get prop
            , prop )
        )


addLabels : (a -> Path.Label) -> List (Property a) -> Set a
addLabels =
    toSet


expand : Property a -> Property a
expand = Property.expand


collapse : Property a -> Property a
collapse = Property.collapse


toChoice : Property a -> Property a
toChoice =
    Property.toChoice


{-| Changes panel shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.shape (cols 2)

    Builder.choice ... |> Buidler.shape (rows 1)

    Builder.choice ... |> Buidler.shape (by 2 3)
-}
shape : PanelShape -> Property a -> Property a
shape = Property.setPanelShape


{-| Changes cell shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.cells single

    Builder.choice ... |> Buidler.shape halfByTwo

    Builder.choice ... |> Buidler.shape halfByHalf
-}
cells : CellShape -> Property a -> Property a
cells = Property.setCellShape
