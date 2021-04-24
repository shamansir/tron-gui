module Tron.Builder.String exposing
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


{-| -}
type alias Builder = B.Tron String


{-| -}
type alias Set = B.Set String


{-| -}
type alias Icon = B.Icon


{-| -}
none : Builder
none = B.none


{-| -}
root : Set -> Builder
root = B.root


{-| -}
float : Axis -> Float -> Builder
float axis current = B.float axis current <| String.fromFloat


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Builder
int axis current = B.int axis current <| String.fromInt


{-| -}
number : Axis -> Float -> Builder
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Builder
xy xAxis yAxis = B.xy xAxis yAxis xyToString


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Builder
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Builder
input toString fromString current = B.input toString fromString current toString


{-| -}
text : String -> Builder
text default = B.text default identity


{-| -}
color : Color -> Builder
color current = B.color current Color.colorToHexWithAlpha


{-| -}
button : Builder
button = B.button <| always ""


{-| -}
buttonWith : Icon -> Builder
buttonWith icon_ = B.buttonWith icon_ <| always ""


{-| -}
colorButton : Color -> Builder
colorButton color_ = B.colorButton color_ <| always ""


{-| -}
toggle : Bool -> Builder
toggle current = B.toggle current (boolToToggle >> toggleToString)


{-| -}
bool : Bool -> Builder
bool = toggle


{-| -}
nest : PanelShape -> CellShape -> Set -> Builder
nest = B.nest


{-| -}
choice
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
choice shape cellShape toLabel options current =
    B.choice shape cellShape toLabel options current toLabel


{-| -}
choiceWithIcons
     : PanelShape
    -> CellShape
    -> ( comparable -> ( Label, Icon ) )
    -> List comparable
    -> comparable
    -> Builder
choiceWithIcons shape cellShape toLabelAndIcon options current =
    B.choiceWithIcons shape cellShape toLabelAndIcon options current (toLabelAndIcon >> Tuple.first)


{-| -}
choiceByCompare
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceByCompare shape cellShape toLabel options current compare =
    B.choiceByCompare shape cellShape toLabel options current compare toLabel


{-| -}
choiceWithIconsByCompare
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceWithIconsByCompare shape cellShape toLabelAndIcon options current compare =
    B.choiceWithIconsByCompare
        shape
        cellShape
        toLabelAndIcon
        options
        current
        compare
        (toLabelAndIcon >> Tuple.first)


{-| -}
strings
     : List String
    -> String
    -> Builder
strings options current =
    B.strings options current identity


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> Builder
labels toLabel options current  =
    B.labels
        toLabel
        options
        current
        (options
            |> List.head
            |> Maybe.map toLabel
            |> Maybe.withDefault ""
        )
         toLabel


{-| -}
palette
     : PanelShape
    -> List Color
    -> Color
    -> Builder
palette shape options current =
    B.palette shape options current Color.colorToHexWithAlpha



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
expand : Builder -> Builder
expand = B.expand


{-| -}
collapse : Builder -> Builder
collapse = B.collapse


{-| -}
addPath : Builder -> B.Tron ( List Int, String )
addPath = Property.addPath >> B.map (Tuple.mapFirst Path.toList)


{-| -}
addLabeledPath : Builder -> B.Tron ( List String, String )
addLabeledPath = Property.addLabeledPath
