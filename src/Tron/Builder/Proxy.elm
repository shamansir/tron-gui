module Tron.Builder.Proxy exposing
    ( Builder, Set, Icon
    , root
    , none, int, float, number, xy, coord, color, text, input, toggle, bool
    , button, buttonWith, colorButton
    , nest, choice, choiceWithIcons, strings, labels, palette
    , choiceByCompare, choiceWithIconsByCompare
    , icon, iconAt, themedIcon, themedIconAt, makeUrl
    , map, mapSet
    , expand, collapse
    , addPath, addLabeledPath
    )


{-|

`Builder ProxyValue` helps to define the interface that works without providing any user message in response to changes, but rather fires the universal `ProxyValue` that helps to understand what is the type of the value and redirect it somewhere, for example to ports, without any special message-driven flow.

See also: `Tron.Builder.map`, `Tron.Expose.Convert.toProxied`, `Tron.Expose.ProxyValue`, `Tron.Expose.Convert.toExposed`, `Tron.Expose.Data.RawOutUpdate`, `addPath`, `addLabeledPath`.

Using `Builder.map`, you may convert the proxy value to anything that fits your case better.

Using `Builder.addPath` or `Builder.addLabeledPath`, you may automatically add the path to the control in the GUI tree so that you may easily know from where the value came.

Please see `Tron.Builder` for the detailed information on Builders and how to use them. Also, there is some information on these Builders in `README`.

All the documentation for the functions below is in the `Tron.Builder` module, those are just aliases to them without the last argument: the handler that converts the value to a user message,
so that is easier and shorter to use `Proxy`-based `Builder` if you don't need any message anyway.

# Builder
@docs Builder, Set

# Root
@docs root

# Items
@docs none, int, float, number, xy, coord, color, text, input, button, buttonWith, colorButton, toggle, bool

# Groups
@docs nest, choice, choiceWithIcons, choiceByCompare, choiceWithIconsByCompare, strings, labels, palette

# Icons
@docs Icon, icon, iconAt, themedIcon, themedIconAt, makeUrl

# Common Helpers
@docs map, mapSet

# Force expand / collapse for nesting
@docs expand, collapse

# Add Path
@docs addPath, addLabeledPath
-}


import Tron.Builder as B

import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)
import Dict

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
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool)
import Tron.Control.Nest exposing (Form(..), ItemId)

import Tron.Expose.ProxyValue exposing (ProxyValue(..))
import Tron.Builder.Choice as Choice


{-| -}
type alias Builder = B.Builder ProxyValue


{-| -}
type alias Set = B.Set ProxyValue


{-| -}
type alias Icon = B.Icon


{-| -}
map : (ProxyValue -> a) -> Builder -> B.Builder a
map = B.map


{-| -}
mapSet : (ProxyValue -> a) -> Set -> B.Set a
mapSet = B.mapSet


{-| -}
none : Builder
none = B.none


{-| -}
root : Set -> Builder
root = B.root


{-| -}
float : Axis -> Float -> Builder
float axis default = B.float axis default FromSlider


{-| -}
int : { min: Int, max : Int, step : Int } -> Int -> Builder
int axis default = B.int axis default (toFloat >> FromSlider)


{-| -}
number : Axis -> Float -> Builder
number = float


{-| -}
xy : ( Axis, Axis ) -> ( Float, Float ) -> Builder
xy xAxis yAxis = B.xy xAxis yAxis FromXY


{-| -}
coord : ( Axis, Axis ) -> ( Float, Float ) -> Builder
coord = xy


{-| -}
input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Builder
input toString fromString current = B.input toString fromString current (toString >> FromInput)


{-| -}
text : String -> Builder
text default = B.text default FromInput


{-| -}
color : Color -> Builder
color current = B.color current FromColor


{-| -}
button : Builder
button = B.button <| always FromButton


{-| -}
buttonWith : Icon -> Builder
buttonWith icon_ = B.buttonWith icon_ <| always FromButton


{-| -}
colorButton : Color -> Builder
colorButton color_ = B.colorButton color_ <| always FromButton


{-| -}
toggle : Bool -> Builder
toggle current = B.toggle current (boolToToggle >> FromToggle)


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
choice shape cellShape toLabel items current =
    Choice.proxyHelper
        ( shape, cellShape )
        (Choice.withButtons toLabel <| always Default)
        items
        current
        (==)


{-| -}
choiceWithIcons
     : PanelShape
    -> CellShape
    -> ( comparable -> ( Label, Icon ) )
    -> List comparable
    -> comparable
    -> Builder
choiceWithIcons shape cellShape toLabelAndIcon items current =
    Choice.proxyHelper
        ( shape, cellShape )
        (Choice.withButtons
            (toLabelAndIcon >> Tuple.first)
            (toLabelAndIcon >> Tuple.second >> WithIcon)
        )
        items
        current
        (==)


{-| -}
choiceByCompare
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceByCompare shape cellShape toLabel =
    Choice.proxyHelper
        ( shape, cellShape )
        (Choice.withButtons toLabel <| always Default)


{-| -}
choiceWithIconsByCompare
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceWithIconsByCompare shape cellShape toLabelAndIcon =
    Choice.proxyHelper
        ( shape, cellShape )
        (Choice.withButtons
            (toLabelAndIcon >> Tuple.first)
            (toLabelAndIcon >> Tuple.second >> WithIcon)
        )


{-| -}
strings
     : List String
    -> String
    -> Builder
strings options current =
    choice
        (cols 1)
        CS.twiceByHalf
        identity
        options
        current


{-| -}
labels
     : ( a -> Label )
    -> List a
    -> a
    -> msg
    -> Builder
labels toLabel options current fallback =
    let
        labelToValue =
            options
                |> List.map (\v -> ( toLabel v, v ) )
                |> Dict.fromList

    in choice
        (cols 1)
        CS.twiceByHalf
        identity
        (options |> List.map toLabel)
        (toLabel current)


{-| -}
palette
     : PanelShape
    -> List Color
    -> Color
    -> Builder
palette shape options current =
    Choice.proxyHelper
        ( shape, CS.half )
        (Choice.withButtons
            Color.colorToHexWithAlpha
            WithColor
        )
        options
        current
        (\cv1 cv2 ->
            case ( cv1 |> Color.toRgba, cv2 |> Color.toRgba ) of
                ( c1, c2 ) ->
                    (c1.red == c2.red) &&
                    (c1.blue == c2.blue) &&
                    (c1.green == c2.green) &&
                    (c1.alpha == c2.alpha)
        )


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
addPath : Builder -> B.Builder ( List Int, ProxyValue )
addPath = Property.addPath >> B.map (Tuple.mapFirst Path.toList)


{-| -}
addLabeledPath : Builder -> B.Builder ( List String, ProxyValue )
addLabeledPath = Property.addLabeledPath
