module Tron.Tree.Build.Unit exposing (..)


import Color exposing (Color)
import Axis exposing (Axis)

import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Tree as T exposing (..)
import Tron.Tree as Tree
import Tron.Tree.Controls as Tree
import Tron.Tree.Build as B
import Tron.Control exposing (Control(..))
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Button exposing (Face(..), Icon(..), Url(..))
import Tron.Control.Impl.Nest exposing (Form(..))



type alias Tree = T.Tree ()


type alias Set = B.Set ()


none : Tree
none = B.none ()


root : Set -> Tree
root props =
    B.root props ()


float : Axis -> Float -> Tree
float axis value =
    B.float axis value ()



int : { min: Int, max : Int, step : Int } -> Int -> Tree
int opts default =
    B.int opts default ()


number : Axis -> Float -> Tree
number axis val = B.number axis val ()


xy : ( Axis, Axis ) -> ( Float, Float ) -> Tree
xy axes value = B.xy axes value ()


coord : ( Axis, Axis ) -> ( Float, Float ) -> Tree
coord axes value = B.coord axes value ()


input : ( x -> String ) -> x -> Tree
input toString value = -- FIXME: accept just `String` and `value`
    B.input toString value ()


text : String -> Tree
text value =
    B.text value ()


color : Color -> Tree
color value =
    B.color value ()


button : Tree
button =
    B.button ()


face : Face -> Tree -> Tree
face = B.face


icon : Url -> Face
icon = B.icon


iconAt : List String -> Face
iconAt = B.iconAt


themedIcon : (Theme -> Url) -> Face
themedIcon = B.themedIcon


themedIconAt : (Theme -> List String) -> Face
themedIconAt = B.themedIconAt


makeUrl : String -> Url
makeUrl = B.makeUrl


-- not exposed
buttonByFace : Face -> Tree
buttonByFace face_ =
    B.buttonByFace face_ ()


toggle : Bool -> Tree
toggle value =
    B.toggle value ()


bool : Bool -> Tree
bool value = B.bool value ()


nest : Set -> Tree
nest items =
    B.nest items ()


useColor : Color -> Face
useColor = B.useColor


choice
     : B.Set comparable
    -> comparable
    -> T.Tree (Int, comparable)
choice = B.choice


choiceBy
     : B.Set a
    -> a
    -> ( a -> a -> Bool )
    -> T.Tree (Int, a)
choiceBy =
    B.choiceBy


strings
     : List String
    -> String
    -> T.Tree (Int, String)
strings = B.strings


labels
     : ( a -> Path.Label )
    -> List a
    -> a
    -> T.Tree ( Int, Path.Label )
labels = B.labels


palette
     : List ( Path.Label, Color )
    -> Color
    -> T.Tree ( Int, Color )
palette = B.palette


buttons : List a -> List (T.Tree a)
buttons =
    B.buttons


toSet : (a -> Path.Label) -> List (T.Tree a) -> Set
toSet toLabel =
    List.map
        (\prop ->
            ( toLabel <| Tree.get prop
            , prop |> Tree.toUnit
            )
        )


addLabels : (a -> Path.Label) -> List (T.Tree a) -> Set
addLabels =
    toSet


expand : Tree -> Tree
expand = B.expand


collapse : Tree -> Tree
collapse = B.collapse


toChoice : Tree -> Tree
toChoice = B.toChoice


{-| Changes panel shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.shape (cols 2)

    Builder.choice ... |> Buidler.shape (rows 1)

    Builder.choice ... |> Buidler.shape (by 2 3)
-}
shape : PanelShape -> Tree -> Tree
shape = B.shape


{-| Changes cell shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.cells single

    Builder.choice ... |> Buidler.shape halfByTwo

    Builder.choice ... |> Buidler.shape halfByHalf
-}
cells : CellShape -> Tree -> Tree
cells = B.cells
