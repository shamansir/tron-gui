module Tron.Builder.Any exposing ( .. )


import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)
import Url.Builder as Url
import Dict

import Tron exposing (Tron, Set)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (expand, collapse)
import Tron.Control exposing (Control(..))
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape exposing (rows, cols)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url(..))
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool)
import Tron.Control.Nest as Nest exposing (Form(..), ItemId)

import Tron.Builder.Choice as Choice



none : Tron a
none = Nil


root : Set a -> a -> Tron a
root props a =
    nest
        props
        a
        |> expand
        |> shape (rows 1)


float : Axis -> Float -> a -> Tron a
float axis value a =
    Number
        <| Control axis ( Nothing, value )
        a



int : { min: Int, max : Int, step : Int } -> Int -> a -> Tron a
int { min, max, step } default a =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        a


number : Axis -> Float -> a -> Tron a
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> a -> Tron a
xy axes value a =
    Coordinate
        <| Control axes ( Nothing, value )
        <| a


coord : ( Axis, Axis ) -> ( Float, Float ) -> a -> Tron a
coord = xy


input : ( x -> String ) -> x -> a -> Tron a
input toString value a = -- FIXME: accept just `String` and `value`
    Text
        <| Control
            ()
            ( Ready, toString value )
            a


text : String -> a -> Tron a
text value a =
    Text
        <| Control
            ()
            ( Ready, value )
            a


color : Color -> a -> Tron a
color value a =
    Color
        <| Control
            ()
            ( Nothing, value )
            a


button : a -> Tron a
button =
    buttonByFace Default


face : Face -> Tron a -> Tron a
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
buttonByFace : Face -> a -> Tron a
buttonByFace face_ a =
    Action
        <| Control
            face_
            ()
            a


toggle : Bool -> a -> Tron a
toggle value a =
    Toggle
        <| Control
            ()
            (boolToToggle value)
            a


bool : Bool -> a -> Tron a
bool = toggle


nest : Set a -> a -> Tron a
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
            a


useColor : Color -> Face
useColor = WithColor


choice
     : Set comparable
    -> comparable
    -> Tron (Int, comparable)
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
    -> Tron (Int, a)
choiceBy set current compare =
    Choice.helper
        Property.defaultNestShape
        set
        current
        compare


strings
     : List Label
    -> Label
    -> Tron (Int, Label)
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
     : ( a -> Label )
    -> List a
    -> a
    -> Tron ( Int, Label )
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
     : List ( Label, Color )
    -> Color
    -> Tron ( Int, Color )
palette options current =
    choiceBy
        (options
            |> buttons
            |> List.map (Tron.with (face << useColor << Tuple.second))
            |> addLabels Tuple.first
            |> Tron.mapSet Tuple.second
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


buttons : List a -> List (Tron a)
buttons =
    List.map button


toSet : (a -> Label) -> List (Tron a) -> Set a
toSet toLabel =
    List.map
        (\prop ->
            Tron.Property.get prop
                |> Maybe.map
                    (\v ->
                        ( toLabel v
                        , prop
                        )
                    )
        )
    >> List.filterMap identity


addLabels : (a -> Label) -> List (Tron a) -> Set a
addLabels =
    toSet


expand : Tron a -> Tron a
expand = Property.expand


collapse : Tron a -> Tron a
collapse = Property.collapse


addPath : Tron a -> Tron ( List Path.Index, a )
addPath = Property.addPath >> Tron.map (Tuple.mapFirst Path.toList)


addLabeledPath : Tron a -> Tron ( List String, a )
addLabeledPath = Property.addLabeledPath


toChoice : Tron a -> Tron a
toChoice =
    Property.toChoice


{-| Changes panel shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.shape (cols 2)

    Builder.choice ... |> Buidler.shape (rows 1)

    Builder.choice ... |> Buidler.shape (by 2 3)
-}
shape : PanelShape -> Tron a -> Tron a
shape = Property.setPanelShape


{-| Changes cell shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.cells single

    Builder.choice ... |> Buidler.shape halfByTwo

    Builder.choice ... |> Buidler.shape halfByHalf
-}
cells : CellShape -> Tron a -> Tron a
cells = Property.setCellShape
