module Tron.Property.Build.Unit exposing (..)


import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)

import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Property as P exposing (..)
import Tron.Property as Property
import Tron.Property.Controls as Property
import Tron.Property.Build as B
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



type alias Property = P.Property ()


type alias Set = B.Set ()


none : Property
none = B.none ()


root : Set -> Property
root props =
    B.root props ()


float : Axis -> Float -> Property
float axis value =
    B.float axis value ()



int : { min: Int, max : Int, step : Int } -> Int -> Property
int opts default =
    B.int opts default ()


number : Axis -> Float -> Property
number axis val = B.number axis val ()


xy : ( Axis, Axis ) -> ( Float, Float ) -> Property
xy axes value = B.xy axes value ()


coord : ( Axis, Axis ) -> ( Float, Float ) -> Property
coord axes value = B.coord axes value ()


input : ( x -> String ) -> x -> Property
input toString value = -- FIXME: accept just `String` and `value`
    B.input toString value ()


text : String -> Property
text value =
    B.text value ()


color : Color -> Property
color value =
    B.color value ()


button : Property
button =
    B.button ()


face : Face -> Property -> Property
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
buttonByFace : Face -> Property
buttonByFace face_ =
    B.buttonByFace face_ ()


toggle : Bool -> Property
toggle value =
    B.toggle value ()


bool : Bool -> Property
bool value = B.bool value ()


nest : Set -> Property
nest items =
    B.nest items ()


useColor : Color -> Face
useColor = B.useColor


choice
     : B.Set comparable
    -> comparable
    -> P.Property (Int, comparable)
choice = B.choice


choiceBy
     : B.Set a
    -> a
    -> ( a -> a -> Bool )
    -> P.Property (Int, a)
choiceBy =
    B.choiceBy


strings
     : List String
    -> String
    -> P.Property (Int, String)
strings = B.strings


labels
     : ( a -> Path.Label )
    -> List a
    -> a
    -> P.Property ( Int, Path.Label )
labels = B.labels


palette
     : List ( Path.Label, Color )
    -> Color
    -> P.Property ( Int, Color )
palette = B.palette


buttons : List a -> List (P.Property a)
buttons =
    B.buttons


toSet : (a -> Path.Label) -> List (P.Property a) -> Set
toSet toLabel =
    List.map
        (\prop ->
            ( toLabel <| Property.get prop
            , prop |> Property.toUnit
            )
        )


addLabels : (a -> Path.Label) -> List (P.Property a) -> Set
addLabels =
    toSet


expand : Property -> Property
expand = B.expand


collapse : Property -> Property
collapse = B.collapse


toChoice : Property -> Property
toChoice = B.toChoice


{-| Changes panel shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.shape (cols 2)

    Builder.choice ... |> Buidler.shape (rows 1)

    Builder.choice ... |> Buidler.shape (by 2 3)
-}
shape : PanelShape -> Property -> Property
shape = B.shape


{-| Changes cell shape for `nest` and `choice` panels:

    Builder.nest ... |> Buidler.cells single

    Builder.choice ... |> Buidler.shape halfByTwo

    Builder.choice ... |> Buidler.shape halfByHalf
-}
cells : CellShape -> Property -> Property
cells = B.cells
