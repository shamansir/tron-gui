module Tron.Builder.Unit exposing
    ( root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool, button
    , nest, choice, choiceBy, strings, labels, palette, buttons
    , face, Face, Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl, useColor
    , live, toChoice, toSet, dontHandle, toSwitch, toKnob
    , expand, collapse, shape, cells
    , addPath, addLabeledPath, addLabels
    )


{-|

`Tron ()` helps to define the interface that works without providing any user message in response to changes.

See also: `Tron.Expose.Convert.toUnit`, `Tron.Expose.Convert.toProxied`, `Tron.Expose.Convert.toExposed`, `Tron.Expose.Convert.toStrExposed`, `addPath`, `addLabeledPath`.

Using the `Tron.Expose.Convert.toExposed` function you may easily convert such `Tron` to the one that stores all the value update information in JSON-friendly format. Or, using the `toStrExposed` from the same module, you may get only the label-path and the stringified value.

Using `Tron.Expose.Convert.toProxied`, and `Builder.addPath` or `Builder.addLabeledPath` after that, you may automatically add proxied value (see `Tron.Control.Value` for details) and the path to the control in the GUI tree so that you may easily know from where the value came.

Please see `Tron.Builder` for the detailed information on Builders and how to use them. Also, there is some information on these Builders in `README`.

All the documentation for the functions below is in the `Tron.Builder` module, those are just aliases to them without the last argument: the handler that converts the value to a user message,
so that is easier and shorter to use `Unit`-based `Builder` if you don't need any message anyway.

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
@docs toSet, toChoice, toSwitch, toKnob, addLabels, dontHandle

# Live
@docs live

# Add Path
@docs addPath, addLabeledPath
-}


import Tron as B
import Tron.Builder.Any as B

import Color exposing (Color)
import Axis exposing (Axis)

import Tron.Control exposing (..)
import Tron.Property as Property exposing (..)
import Tron.Control exposing (Control(..))
import Tron.Util exposing (findMap)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape as Shape exposing (rows, cols)
import Tron.Style.Theme exposing (Theme)
import Tron.Builder as BB


-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url)
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool, toggleToString)
import Tron.Control.XY exposing (xyToString, xyFromString)
import Tron.Control.Nest as Nest exposing (Form(..))


{-| -}
type alias Tron = B.Tron ()


{-| -}
type alias Set = B.Set ()


{-| -}
type alias Icon = Button.Icon


{-| -}
type alias Face = Button.Face


{-| -}
none : Tron
none = B.none


{-| -}
root : Set -> Tron
root set = B.root set ()


{-| -}
float : Axis -> Float -> Tron
float axis current = B.float axis current ()


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Tron
int axis current = B.int axis current ()


{-| -}
number : Axis -> Float -> Tron
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Tron
xy xAxis yAxis = B.xy xAxis yAxis ()


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Tron
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Tron
input toString fromString current = B.input toString current ()


{-| -}
text : String -> Tron
text default = B.text default ()


{-| -}
color : Color -> Tron
color current = B.color current ()


{-| -}
button : Tron
button = B.button ()


{-| -}
toggle : Bool -> Tron
toggle current = B.toggle current ()


{-| -}
bool : Bool -> Tron
bool = toggle


{-| -}
nest : Set -> Tron
nest set = B.nest set ()


{-| -}
choice
     : B.Set comparable
    -> comparable
    -> Tron
choice items current =
    B.choice items current |> Property.map (always ())


{-| -}
choiceBy
     : B.Set a
    -> a
    -> ( a -> a -> Bool )
    -> Tron
choiceBy items current compare =
    B.choiceBy items current compare |> Property.map (always ())



{-| -}
strings
     : List String
    -> String
    -> Tron
strings options current =
    B.strings options current |> Property.map (always ())


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tron
labels toLabel options current =
    B.labels toLabel options current |> Property.map (always ())


{-| -}
palette
     : List ( Label, Color )
    -> Color
    -> Tron
palette colors currentColor =
    B.palette colors currentColor |> Property.map (always ())


{-| -}
useColor : Color -> Face
useColor = B.useColor


{-| -}
face : Face -> B.Tron a -> B.Tron a
face = B.face


{-| -}
buttons : List a -> List (B.Tron a)
buttons = B.buttons


{-| -}
toSet : (a -> Label) -> List (B.Tron a) -> B.Set a
toSet = B.toSet


{-| -}
addLabels : (a -> Label) -> List (B.Tron a) -> B.Set a
addLabels = B.addLabels



{-| The replacement for `handleWith` since we don't handle anything for () -}
dontHandle : B.Tron a -> Tron
dontHandle = B.map <| always ()


{-| -}
icon : Url -> Face
icon = B.icon


{-| -}
themedIcon : (Theme -> Url) -> Face
themedIcon = B.themedIcon


{-| -}
iconAt : List String -> Face
iconAt = B.iconAt


{-| -}
themedIconAt : (Theme -> List String) -> Face
themedIconAt = B.themedIconAt


{-| -}
makeUrl : String -> Url
makeUrl = Button.makeUrl


{-| -}
expand : B.Tron a -> B.Tron a
expand = B.expand


{-| -}
collapse : B.Tron a -> B.Tron a
collapse = B.collapse


{-| -}
addPath : Tron -> B.Tron ( List Int, () )
addPath = B.addPath


{-| -}
addLabeledPath : Tron -> B.Tron ( List String, () )
addLabeledPath = B.addLabeledPath


{-| -}
toChoice : Tron -> Tron
toChoice = B.toChoice


{-| -}
toSwitch : Tron -> Tron
toSwitch = Property.setForm Nest.SwitchThrough


{-| -}
toKnob : Tron -> Tron
toKnob = Property.setForm Nest.Knob


{-| -}
live : Tron -> Tron
live = Property.Live


{-| -}
shape : PanelShape -> B.Tron a -> B.Tron a
shape = B.shape


{-| -}
cells : CellShape -> B.Tron a -> B.Tron a
cells = B.cells
