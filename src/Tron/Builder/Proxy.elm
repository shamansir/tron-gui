module Tron.Builder.Proxy exposing
    ( root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool, button
    , nest, choice, choiceBy, strings, labels, palette, buttons
    , face, Face, Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl, useColor
    , live, toChoice, toSet, autoHandle, toSwitch, toKnob
    , expand, collapse, shape, cells
    , addPath, addLabeledPath, addLabels
    )


{-|

`Tron Value` helps to define the interface that works without providing any user message in response to changes, but rather fires the universal `Value` that helps to understand what is the type of the value and redirect it somewhere, for example to ports, without any special message-driven flow.

See also: `Tron.Builder.map`, `Tron.Expose.Convert.toProxied`, `Tron.Control.Value`, `Tron.Expose.Convert.toExposed`, `Tron.Expose.Data.RawOutUpdate`, `addPath`, `addLabeledPath`.

Using `Tron.map`, you may convert the proxy value to anything that fits your case better.

Using `Builder.addPath` or `Builder.addLabeledPath`, you may automatically add the path to the control in the GUI tree so that you may easily know from where the value came.

Please see `Tron.Builder` for the detailed information on Builders and how to use them. Also, there is some information on these Builders in `README`.

All the documentation for the functions below is in the `Tron.Builder` module, those are just aliases to them without the last argument: the handler that converts the value to a user message,
so that is easier and shorter to use `Proxy`-based `Builder` if you don't need any message anyway.

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

# Conversion
@docs toSet, toChoice, addLabels, autoHandle

# Add Path
@docs addPath, addLabeledPath
-}


import Tron.Deferred as Def
import Tron.Builder.Any as B
import Tron.Builder as BB

import Color exposing (Color)
import Axis exposing (Axis)

import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property
import Tron.Control exposing (Control(..))
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url)
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool)
import Tron.Control.Nest as Nest exposing (Form(..), ItemId)

import Tron.Control.Value as Value exposing (Value(..))
import Tron.Builder.Choice as Choice
import Tron.Expose.Convert exposing (toProxied)


{-| -}
type alias Tron = Def.Tron Value


{-| -}
type alias Set = Def.Set Value


{-| -}
type alias Icon = Button.Icon


{-| -}
type alias Face = Button.Face


{-| -}
none : Tron
none = B.none


{-| -}
root : Set -> Tron
root set = B.root set Just


{-| -}
float : Axis -> Float -> Tron
float axis default = B.float axis default Just


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Tron
int axis default = B.int axis default Just


{-| -}
number : Axis -> Float -> Tron
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Tron
xy xAxis yAxis = B.xy xAxis yAxis Just


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Tron
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Tron
input toString fromString current = B.input toString current Just


{-| -}
text : String -> Tron
text default = B.text default Just


{-| -}
color : Color -> Tron
color current = B.color current Just


{-| -}
button : Tron
button = B.button Just


{-| -}
toggle : Bool -> Tron
toggle current = B.toggle current Just


{-| -}
bool : Bool -> Tron
bool = toggle


{-| -}
nest : Set -> Tron
nest set = B.nest set Just


{-| -}
choice
     : Def.Set comparable
    -> comparable
    -> Tron
choice set current =
    Choice.helperProxy
        Property.defaultNestShape
        set
        current
        (==)


{-| -}
choiceBy
     : Def.Set a
    -> a
    -> ( a -> a -> Bool )
    -> Tron
choiceBy set current compare =
    Choice.helperProxy
        Property.defaultNestShape
        set
        current
        compare


{-| -}
strings
     : List String
    -> String
    -> Tron
strings options current =
    B.strings options current
        |> Property.map (always Just)


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tron
labels toLabel options current =
    B.labels toLabel options current
        |> Property.map (always Just)


{-| -}
palette
     : List ( Label, Color )
    -> Color
    -> Tron
palette colors currentColor =
    B.palette colors currentColor
        |> Property.map (always Just)


{-| -}
useColor : Color -> Face
useColor = B.useColor


{-| -}
face : Face -> Def.Tron a -> Def.Tron a
face = B.face


{-| -}
buttons : List a -> List (Def.Tron a)
buttons =
    B.buttons
        >> List.map (Property.map <| always << Just)


{-| -}
toSet : (a -> Label) -> List (Def.Tron a) -> Def.Set a
toSet = BB.toSet


{-| -}
addLabels : (a -> Label) -> List (Def.Tron a) -> Def.Set a
addLabels = BB.addLabels


{-| The replacement for `handleWith` since we convert everything automatically for Proxy. -}
autoHandle : Def.Tron a -> Tron
autoHandle =
    Property.map <| always Just


{-| -}
icon : Url -> Icon
icon = Button.icon


{-| -}
themedIcon : (Theme -> Url) -> Icon
themedIcon = Button.themedIcon


{-| -}
iconAt : List String -> Icon
iconAt = Button.iconAt


{-| -}
themedIconAt : (Theme -> List String) -> Icon
themedIconAt = Button.themedIconAt


{-| -}
makeUrl : String -> Url
makeUrl = Button.makeUrl


{-| -}
expand : Def.Tron a -> Def.Tron a
expand = B.expand


{-| -}
collapse : Def.Tron a -> Def.Tron a
collapse = B.collapse


{-| -}
addPath : Tron -> Def.Tron ( List Int, Value )
addPath = BB.addPath


{-| -}
addLabeledPath : Tron -> Def.Tron ( List String, Value )
addLabeledPath = BB.addLabeledPath


{-| -}
toChoice : Tron -> Tron
toChoice = B.toChoice


{-| -}
toSwitch : Tron -> Tron
toSwitch = Property.setChoiceMode Nest.SwitchThrough


{-| -}
toKnob : Tron -> Tron
toKnob = Property.setChoiceMode Nest.Knob


{-| -}
live : Tron -> Tron
live = Property.Live


{-| -}
shape : PanelShape -> Def.Tron a -> Def.Tron a
shape = B.shape


{-| -}
cells : CellShape -> Def.Tron a -> Def.Tron a
cells = B.cells
