module Tron.Builder.String exposing
    ( root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool, button
    , nest, choice, choiceBy, strings, labels, palette, buttons
    , face, Face, Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl, useColor
    , toChoice, toSet, autoHandle
    , expand, collapse, shape, cells
    , addPath, addLabeledPath, addLabels
    )


{-|

`Tron String` helps to define the interface that works without providing any user message in response to changes, but rather fires the stringified value.

See also: `Tron.Expose.Convert.toStrExposed`, `addPath`, `addLabeledPath`.

Please see `Tron.Builder` for the detailed information on Builders and how to use them. Also, there is some information on these Builders in `README`.

Using `Builder.addPath` or `Builder.addLabeledPath`, you may automatically add the path to the control in the GUI tree so that you may easily know from where the value came.

All the documentation for the functions below is in the `Tron.Builder` module, those are just aliases to them without the last argument: the handler that converts the value to a user message,
so that is easier and shorter to use `String`-based `Builder` if you don't need any message anyway.


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

import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)

import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (expand, collapse)
import Tron.Control exposing (Control(..))
import Tron.Util exposing (findMap)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape as Shape exposing (find, rows, cols)
import Tron.Style.Theme exposing (Theme)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button  exposing (Face(..), Icon(..), Url)
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool, toggleToString)
import Tron.Control.XY exposing (xyToString, xyFromString)
import Tron.Control.Nest exposing (Form(..), ItemId)

import Tron.Expose.Convert as Exp


{-| -}
type alias Tron = B.Tron String


{-| -}
type alias Set = B.Set String


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
float axis current = B.float axis current <| String.fromFloat


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Tron
int axis current = B.int axis current <| String.fromInt


{-| -}
number : Axis -> Float -> Tron
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Tron
xy xAxis yAxis = B.xy xAxis yAxis xyToString


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Tron
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Tron
input toString fromString current = B.input toString fromString current toString


{-| -}
text : String -> Tron
text default = B.text default identity


{-| -}
color : Color -> Tron
color current = B.color current Color.colorToHexWithAlpha


{-| -}
button : Tron
button = B.button <| always ""


{-| -}
toggle : Bool -> Tron
toggle current = B.toggle current (boolToToggle >> toggleToString)


{-| -}
bool : Bool -> Tron
bool = toggle


{-| -}
nest : Set -> Tron
nest = B.nest


{-| -}
choice
     : ( comparable -> Label )
    -> B.Set comparable
    -> comparable
    -> Tron
choice toLabel items current =
    B.choice items current toLabel


{-| -}
choiceBy
     : ( a -> Label )
    -> B.Set a
    -> a
    -> ( a -> a -> Bool )
    -> Tron
choiceBy toLabel items current compare =
    B.choiceBy items current compare toLabel



{-| -}
strings
     : List String
    -> String
    -> Tron
strings options current =
    B.strings options current identity


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tron
labels toLabel options current =
    B.labels toLabel options current "" toLabel


{-| -}
palette
     : List Color
    -> Color
    -> Tron
palette colors currentColor =
    B.palette colors currentColor Color.colorToHexWithAlpha


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


{-| The replacement for `handleWith` since we convert everything automatically for Proxy -}
autoHandle : B.Set a -> Set
autoHandle =
    List.map <| Tuple.mapSecond <| Exp.toStrExposed >> Property.map (Tuple.first >> Tuple.second)


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
addPath : Tron -> B.Tron ( List Int, String )
addPath = B.addPath


{-| -}
addLabeledPath : Tron -> B.Tron ( List String, String )
addLabeledPath = B.addLabeledPath


{-| -}
toChoice : Tron -> Tron
toChoice = B.toChoice String.fromInt


{-| -}
shape : PanelShape -> B.Tron a -> B.Tron a
shape = B.shape


{-| -}
cells : CellShape -> B.Tron a -> B.Tron a
cells = B.cells
