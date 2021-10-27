module Tron.Tree.Build.Unit exposing
    ( Set
    , root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool, button, buttonWith
    , nest, choice, choiceBy, strings, labels, palette, buttons
    , face, Face, Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl, useColor
    , live, toChoice, toSet, toSwitch, toKnob
    , expand, collapse, shape, cells
    )

{-|

# Sets
@docs Set, mapSet, toSet

# Root
@docs root

# Items
@docs none, int, float, number, xy, coord, color, text, input, button, toggle, bool

# Groups
@docs nest, choice, choiceBy, strings, labels, palette

# Buttons
@docs buttons, useColor, face, Face

# Icons
@docs Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl

# Force expand / collapse for nesting
@docs expand, collapse

# Shape
@docs shape, cells

# Live

Usually in your `for` function you set the default value to the control, but if you change the control with `live`, then you'll be able to pass some dynamic value from your model to it.

@docs live

# Conversion between types of controls + helpers
@docs toSet, toChoice, toKnob, toSwitch, addLabels, handleWith

# Add Path
@docs addPath, addLabeledPath
-}


import Color exposing (Color)
import Axis exposing (Axis)

import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Tree as T exposing (..)
import Tron.Tree.Build.Any as B
import Tron.Control exposing (Control(..))
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Button exposing (Icon(..), Url(..))
import Tron.Control.Impl.Button as Button exposing (Face)
import Tron.Control.Impl.Nest exposing (Form(..))



type alias Tree = T.Tree ()


type alias Set = List (Label, Tree)


type alias Label = Path.Label


type alias Face = Button.Face


type alias Icon = Button.Icon


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


face : Face -> T.Tree a -> T.Tree a
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
buttonWith : Face -> Tree
buttonWith face_ =
    B.buttonWith face_ ()


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
    -> Tree
choice set current =
    B.choice set current
        |> T.toUnit


choiceBy
     : B.Set a
    -> a
    -> ( a -> a -> Bool )
    -> Tree
choiceBy set a cmp =
    B.choiceBy set a cmp
        |> T.toUnit


strings
     : List String
    -> String
    -> Tree
strings list current =
    B.strings list current
        |> T.toUnit


labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tree
labels toLabel items current =
    B.labels toLabel items current
        |> T.toUnit


palette
     : List ( Label, Color )
    -> Color
    -> Tree
palette items current =
    B.palette items current
        |> T.toUnit


buttons : List a -> List (T.Tree a)
buttons =
    B.buttons


toSet : (a -> Label) -> List (T.Tree a) -> B.Set a
toSet = B.toSet


expand : Tree -> Tree
expand = B.expand


collapse : Tree -> Tree
collapse = B.collapse


toChoice : Tree -> Tree
toChoice = B.toChoice


{-| Convert choice control to a switch by click form:

    Build.choice ... |> Build.toSwitch
-}
toSwitch : Tree -> Tree
toSwitch = B.toSwitch


{-| Convert choice control to a switch by click form:

    Build.choice ... |> Build.toSwitch
-}
toKnob : Tree -> Tree
toKnob = B.toKnob


{-| Changes panel shape for `nest` and `choice` panels:

    Build.nest ... |> Buidler.shape (cols 2)

    Build.choice ... |> Buidler.shape (rows 1)

    Build.choice ... |> Buidler.shape (by 2 3)
-}
shape : PanelShape -> Tree -> Tree
shape = B.shape


{-| Changes cell shape for `nest` and `choice` panels:

    Build.nest ... |> Buidler.cells single

    Build.choice ... |> Buidler.shape halfByTwo

    Build.choice ... |> Buidler.shape halfByHalf
-}
cells : CellShape -> Tree -> Tree
cells = B.cells



live : Tree -> Tree
live = B.live