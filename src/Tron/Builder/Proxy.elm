module Tron.Builder.Proxy exposing
    ( root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool, button
    , nest, choice, choiceBy, strings, labels, palette, buttons
    , face, Face, Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl, useColor
    , toChoice, toSet, autoHandle
    , expand, collapse, shape, cells
    , addPath, addLabeledPath, addLabels
    )


{-|

`Tron ProxyValue` helps to define the interface that works without providing any user message in response to changes, but rather fires the universal `ProxyValue` that helps to understand what is the type of the value and redirect it somewhere, for example to ports, without any special message-driven flow.

See also: `Tron.Builder.map`, `Tron.Expose.Convert.toProxied`, `Tron.Expose.ProxyValue`, `Tron.Expose.Convert.toExposed`, `Tron.Expose.Data.RawOutUpdate`, `addPath`, `addLabeledPath`.

Using `Tron.map`, you may convert the proxy value to anything that fits your case better.

Using `Builder.addPath` or `Builder.addLabeledPath`, you may automatically add the path to the control in the GUI tree so that you may easily know from where the value came.

Please see `Tron.Builder` for the detailed information on Builders and how to use them. Also, there is some information on these Builders in `README`.

All the documentation for the functions below is in the `Tron.Builder` module, those are just aliases to them without the last argument: the handler that converts the value to a user message,
so that is easier and shorter to use `Proxy`-based `Builder` if you don't need any message anyway.

# Root
@docs root

# Items
@docs none, int, float, number, xy, coord, color, text, input, button, buttonWith, colorButton, toggle, bool

# Groups
@docs nest, choice, choiceBy, strings, labels, palette

# Buttons
@docs buttons, buttonsWithIcons, coloredButtons, setColor

# Icons
@docs Icon, setIcon, icon, iconAt, themedIcon, themedIconAt, makeUrl

# Force expand / collapse for nesting
@docs expand, collapse

# Conversion
@docs toSet, toChoice, addLabels, autoHandle

# Add Path
@docs addPath, addLabeledPath
-}


import Tron as B
import Tron.Builder as B

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
import Tron.Control.Nest exposing (Form(..), ItemId)

import Tron.Expose.ProxyValue exposing (ProxyValue(..))
import Tron.Builder.Choice as Choice
import Tron.Expose.Convert exposing (toProxied)


{-| -}
type alias Tron = B.Tron ProxyValue


{-| -}
type alias Set = B.Set ProxyValue


{-| -}
type alias Icon = B.Icon


{-| -}
type alias Face = B.Face


{-| -}
none : Tron
none = B.none


{-| -}
root : Set -> Tron
root = B.root


{-| -}
float : Axis -> Float -> Tron
float axis default = B.float axis default FromSlider


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Tron
int axis default = B.int axis default (toFloat >> FromSlider)


{-| -}
number : Axis -> Float -> Tron
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Tron
xy xAxis yAxis = B.xy xAxis yAxis FromXY


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Tron
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Tron
input toString fromString current = B.input toString fromString current (toString >> FromInput)


{-| -}
text : String -> Tron
text default = B.text default FromInput


{-| -}
color : Color -> Tron
color current = B.color current FromColor


{-| -}
button : Tron
button = B.button <| always FromButton


{-| -}
toggle : Bool -> Tron
toggle current = B.toggle current (boolToToggle >> FromToggle)


{-| -}
bool : Bool -> Tron
bool = toggle


{-| -}
nest : Set -> Tron
nest = B.nest


{-| -}
choice
     : B.Set comparable
    -> comparable
    -> Tron
choice items current =
    B.choice items current (always ())
        |> toProxied |> Property.map Tuple.first


{-| -}
choiceBy
     : B.Set a
    -> a
    -> ( a -> a -> Bool )
    -> Tron
choiceBy items current compare =
    B.choiceBy items current compare (always ())
        |> toProxied |> Property.map Tuple.first



{-| -}
strings
     : List String
    -> String
    -> Tron
strings options current =
    B.strings options current (always ())
        |> toProxied |> Property.map Tuple.first


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tron
labels toLabel options current =
    B.labels toLabel options current () (always ())
        |> toProxied |> Property.map Tuple.first


{-| -}
palette
     : List ( Label, Color )
    -> Color
    -> Tron
palette colors currentColor =
    B.palette colors currentColor (always ())
        |> toProxied |> Property.map Tuple.first


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


{-| The replacement for `handleWith` since we convert everything automatically for Proxy. -}
autoHandle : B.Tron a -> Tron
autoHandle =
    toProxied >> Property.map Tuple.first


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
expand : B.Tron a -> B.Tron a
expand = B.expand


{-| -}
collapse : B.Tron a -> B.Tron a
collapse = B.collapse


{-| -}
addPath : Tron -> B.Tron ( List Int, ProxyValue )
addPath = B.addPath


{-| -}
addLabeledPath : Tron -> B.Tron ( List String, ProxyValue )
addLabeledPath = B.addLabeledPath


{-| -}
toChoice : Tron -> Tron
toChoice = B.toChoice FromChoice


{-| -}
shape : PanelShape -> B.Tron a -> B.Tron a
shape = B.shape


{-| -}
cells : CellShape -> B.Tron a -> B.Tron a
cells = B.cells
