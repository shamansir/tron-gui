module Tron.Builder.Unit exposing
    ( root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool
    , button, buttonWith, colorButton
    , nest, choice, choiceByCompare, strings, labels, palette
    , buttons, buttonsWithIcons, coloredButtons, setColor
    , Icon, addIcon, icon, iconAt, themedIcon, themedIconAt, makeUrl
    , toChoice, toSet, handleWith
    , expand, collapse
    , addPath, addLabeledPath, addLabels
    )


{-|

`Tron ()` helps to define the interface that works without providing any user message in response to changes.

See also: `Tron.Expose.Convert.toUnit`, `Tron.Expose.Convert.toProxied`, `Tron.Expose.Convert.toExposed`, `Tron.Expose.Convert.toStrExposed`, `addPath`, `addLabeledPath`.

Using the `Tron.Expose.Convert.toExposed` function you may easily convert such `Tron` to the one that stores all the value update information in JSON-friendly format. Or, using the `toStrExposed` from the same module, you may get only the label-path and the stringified value.

Using `Tron.Expose.Convert.toProxied`, and `Builder.addPath` or `Builder.addLabeledPath` after that, you may automatically add proxied value (see `Tron.Expose.ProxyValue` for details) and the path to the control in the GUI tree so that you may easily know from where the value came.

Please see `Tron.Builder` for the detailed information on Builders and how to use them. Also, there is some information on these Builders in `README`.

All the documentation for the functions below is in the `Tron.Builder` module, those are just aliases to them without the last argument: the handler that converts the value to a user message,
so that is easier and shorter to use `Unit`-based `Builder` if you don't need any message anyway.

# Root
@docs root

# Items
@docs none, int, float, number, xy, coord, color, text, input, button, buttonWith, colorButton, toggle, bool

# Groups
@docs nest, choice, choiceByCompare, strings, labels, palette

# Buttons
@docs buttons, buttonsWithIcons, coloredButtons, setColor

# Icons
@docs Icon, addIcon, icon, iconAt, themedIcon, themedIconAt, makeUrl

# Force expand / collapse for nesting
@docs expand, collapse

# Conversion
@docs toSet, toChoice, addLabels, handleWith

# Add Path
@docs addPath, addLabeledPath
-}


import Tron as B
import Tron.Builder as B

import Array
import Color exposing (Color)
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
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url)
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool, toggleToString)
import Tron.Control.XY exposing (xyToString, xyFromString)
import Tron.Control.Nest exposing (Form(..))


{-| -}
type alias Tron = B.Tron ()


{-| -}
type alias Set = B.Set ()


{-| -}
type alias Icon = B.Icon



{-| -}
none : Tron
none = B.none


{-| -}
root : Set -> Tron
root = B.root


{-| -}
float : Axis -> Float -> Tron
float axis current = B.float axis current <| always ()


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Tron
int axis current = B.int axis current <| always ()


{-| -}
number : Axis -> Float -> Tron
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Tron
xy xAxis yAxis = B.xy xAxis yAxis <| always ()


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Tron
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Tron
input toString fromString current = B.input toString fromString current <| always ()


{-| -}
text : String -> Tron
text default = B.text default <| always ()


{-| -}
color : Color -> Tron
color current = B.color current <| always ()


{-| -}
button : Tron
button = B.button <| always ()


{-| -}
buttonWith : Icon -> Tron
buttonWith icon_ = B.buttonWith icon_ <| always ()


{-| -}
colorButton : Color -> Tron
colorButton color_ = B.colorButton color_ <| always ()


{-| -}
toggle : Bool -> Tron
toggle current = B.toggle current <| always ()


{-| -}
bool : Bool -> Tron
bool = toggle


{-| -}
nest : PanelShape -> CellShape -> Set -> Tron
nest = B.nest


{-| -}
choice
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Tron
choice shape cellShape toLabel options current =
    B.choice shape cellShape toLabel options current <| always ()


{-| -}
choiceWithIcons
     : PanelShape
    -> CellShape
    -> ( comparable -> ( Label, Icon ) )
    -> List comparable
    -> comparable
    -> Tron
choiceWithIcons shape cellShape toLabelAndIcon options current =
    B.choiceWithIcons shape cellShape toLabelAndIcon options current <| always ()


{-| -}
choiceByCompare
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Tron
choiceByCompare shape cellShape toLabel options current compare =
    B.choiceByCompare shape cellShape toLabel options current compare <| always ()


{-| -}
choiceWithIconsByCompare
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Tron
choiceWithIconsByCompare shape cellShape toLabelAndIcon options current compare =
    B.choiceWithIconsByCompare shape cellShape toLabelAndIcon options current compare <| always ()


{-| -}
strings
     : List String
    -> String
    -> Tron
strings options current =
    B.strings options current <| always ()


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> Tron
labels toLabel options current =
    B.labels toLabel options current () <| always ()


{-| -}
palette
     : PanelShape
    -> List Color
    -> Color
    -> Tron
palette shape options current =
    B.palette shape options current <| always ()


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
expand : Tron -> Tron
expand = B.expand


{-| -}
collapse : Tron -> Tron
collapse = B.collapse


{-| -}
addPath : Tron -> B.Tron ( List Int, () )
addPath = Property.addPath >> B.map (Tuple.mapFirst Path.toList)


{-| -}
addLabeledPath : Tron -> B.Tron ( List String, () )
addLabeledPath = Property.addLabeledPath
