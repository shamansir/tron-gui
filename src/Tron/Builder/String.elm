module Tron.Builder.String exposing (..)


import Tron.Builder as B

import Array
import Color exposing (Color)
import Color.Convert as Color
import Axis exposing (Axis)

import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (expand, collapse)
import Tron.Control exposing (Control(..))
import Tron.Util exposing (findMap)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Style.PanelShape as Shape exposing (find, rows, cols)

-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button  exposing (Face(..), Icon(..), Url)
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool, toggleToString)
import Tron.Control.XY exposing (xyToString, xyFromString)
import Tron.Control.Nest exposing (Form(..), ItemId)


type alias Builder = B.Builder String


type alias Set = B.Set String


none : Builder
none = B.none


root : Set -> Builder
root = B.root


float : Axis -> Float -> Builder
float axis current = B.float axis current <| String.fromFloat


int : { min: Int, max : Int, step : Int } -> Int -> Builder
int axis current = B.int axis current <| String.fromInt


number : Axis -> Float -> Builder
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> Builder
xy xAxis yAxis = B.xy xAxis yAxis xyToString


coord : ( Axis, Axis ) -> ( Float, Float ) -> Builder
coord = xy


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Builder
input toString fromString current = B.input toString fromString current toString


text : String -> Builder
text default = B.text default identity


color : Color -> Builder
color current = B.color current Color.colorToHexWithAlpha


button : Builder
button = B.button <| always ""


buttonWith : Icon -> Builder
buttonWith icon_ = B.buttonWith icon_ <| always ""


toggle : Bool -> Builder
toggle current = B.toggle current (boolToToggle >> toggleToString)


bool : Bool -> Builder
bool = toggle


nest : PanelShape -> CellShape -> Set -> Builder
nest = B.nest


choice
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choice pShape cShape toLabel items current compare =
    B.choice pShape cShape toLabel items current compare toLabel


choiceIcons
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceIcons pShape cShape toLabel items current compare =
    B.choiceIcons pShape cShape toLabel items current compare (toLabel >> Tuple.first)


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
choiceAuto pShape cShape toLabel items current =
    B.choiceAuto pShape cShape toLabel items current toLabel



strings
     : List String
    -> String
    -> Builder
strings options current =
    B.strings options current identity


labels
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
labels toLabel items current compare =
    B.labels toLabel items current compare toLabel


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
labelsAuto toLabel items current =
    B.labelsAuto toLabel items current toLabel


palette
     : PanelShape
    -> List Color
    -> Color
    -> Builder
palette shape options current =
    B.palette shape options current Color.colorToHexWithAlpha


{-| Create an `Icon` from its URL or filename.

    import Url.Builder as Url

    Builder.icon
        <| makeUrl <| Url.relative [ "assets", "myicon.svg" ] []
-}
icon : Url -> Icon
icon = Button.icon


{-| Create an `Icon` from its URL or filename.

    import Url.Builder as Url

    Builder.themedIcon
        <| \theme ->
            makeUrl <| Url.relative [ "assets", "myicon_" ++ Theme.toString theme ++ ".svg" ] []
-}
themedIcon : (Theme -> Url) -> Icon
themedIcon = Button.themedIcon


{-| Make URL from String
-}
makeUrl : String -> Url
makeUrl = Button.makeUrl


{-| Forcefully expand the nesting:

    Builder.nest ... |> Builder.expand
    Builder.choice ... |> Builder.expand
-}
expand : Builder -> Builder
expand = B.expand


{-| Forcefully collapse the nesting:

    Builder.nest ... |> Builder.collapse
    Builder.choice ... |> Builder.collapse
-}
collapse : Builder -> Builder
collapse = B.collapse
