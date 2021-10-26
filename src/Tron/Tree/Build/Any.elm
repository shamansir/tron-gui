module Tron.Tree.Build.Any exposing (..)


import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)

import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Tree.Internals as Tree
import Tron.Tree.Controls as Tree
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

import Tron.Tree.Build.Choice as Choice


type alias Tree a = Tree.Tree a


type alias Set a = List (Label, Tree a)


type alias Face = Button.Face


type alias Label = Path.Label


mapSet : (a -> b) -> Set a -> Set b
mapSet =
    List.map << Tuple.mapSecond << Tree.map


none : a -> Tree a
none = Tree.Nil


root : Set a -> a -> Tree a
root props a =
    nest
        props
        a
        |> expand
        |> shape (rows 1)


float : Axis -> Float -> a -> Tree a
float axis value a =
    Tree.Number
        <| Control axis ( Nothing, value )
        <| a



int : { min: Int, max : Int, step : Int } -> Int -> a -> Tree a
int { min, max, step } default a =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        a


number : Axis -> Float -> a -> Tree a
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> a -> Tree a
xy axes value a =
    Tree.Coordinate
        <| Control axes ( Nothing, value )
        <| a


coord : ( Axis, Axis ) -> ( Float, Float ) -> a -> Tree a
coord = xy


input : ( x -> String ) -> x -> a -> Tree a
input toString value a = -- FIXME: accept just `String` and `value`
    Tree.Text
        <| Control
            ()
            ( Ready, toString value )
        <| a


text : String -> a -> Tree a
text value a =
    Tree.Text
        <| Control
            ()
            ( Ready, value )
        <| a


color : Color -> a -> Tree a
color value a =
    Tree.Color
        <| Control
            ()
            ( Nothing, value )
        <| a


button : a -> Tree a
button =
    buttonByFace Default


face : Face -> Tree a -> Tree a
face =
    Tree.setFace


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
buttonByFace : Face -> a -> Tree a
buttonByFace face_ a =
    Tree.Action
        <| Control
            face_
            ()
        <| a


toggle : Bool -> a -> Tree a
toggle value a =
    Tree.Toggle
        <| Control
            ()
            (boolToToggle value)
        <| a


bool : Bool -> a -> Tree a
bool = toggle


nest : Set a -> a -> Tree a
nest items a =
    Tree.Group
        Nothing
        Tree.defaultNestShape
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
    -> Tree (Int, comparable)
choice set current =
    Choice.helper
        Tree.defaultNestShape
        set
        current
        (==)


choiceBy
     : Set a
    -> a
    -> ( a -> a -> Bool )
    -> Tree (Int, a)
choiceBy set current compare =
    Choice.helper
        Tree.defaultNestShape
        set
        current
        compare


strings
     : List String
    -> String
    -> Tree (Int, String)
strings options current =
    choice
        (options
            |> buttons
            |> toSet identity
        )
        current
    |> shape (cols 1)
    |> cells CS.twiceByHalf


labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tree ( Int, Label )
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
            |> toSet identity
        )
        (toLabel current)
        |> shape (cols 1)
        |> cells CS.twiceByHalf


palette
     : List ( Label, Color )
    -> Color
    -> Tree ( Int, Color )
palette options current =
    choiceBy
        (options
            |> buttons
            |> List.map (Tree.with (face << useColor << Tuple.second))
            |> toSet Tuple.first
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


buttons : List a -> List (Tree a)
buttons =
    List.map button


toSet : (a -> Label) -> List (Tree a) -> Set a
toSet toLabel =
    List.map
        (\prop ->
            ( toLabel <| Tree.get prop
            , prop
            )
        )


expand : Tree a -> Tree a
expand = Tree.expand


collapse : Tree a -> Tree a
collapse = Tree.collapse


toChoice : Tree a -> Tree a
toChoice =
    Tree.toChoice


{-| Changes panel shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.shape (cols 2)

    Builder.choice ... |> Buidler.shape (rows 1)

    Builder.choice ... |> Buidler.shape (by 2 3)
-}
shape : PanelShape -> Tree a -> Tree a
shape = Tree.setPanelShape


{-| Changes cell shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.cells single

    Builder.choice ... |> Buidler.shape halfByTwo

    Builder.choice ... |> Buidler.shape halfByHalf
-}
cells : CellShape -> Tree a -> Tree a
cells = Tree.setCellShape


live : Tree a -> Tree a
live prop =
    case prop of
        Tree.Live _ -> prop
        _ -> Tree.Live prop