module Tron.Builder.Unit exposing (..)


import Tron.Builder as B

import Array
import Color exposing (Color)
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
import Tron.Style.Theme exposing (Theme)


-- TODO: make controls init themselves, so get rid of these imports below
import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url)
import Tron.Control.Toggle exposing (boolToToggle, toggleToBool, toggleToString)
import Tron.Control.XY exposing (xyToString, xyFromString)
import Tron.Control.Nest exposing (Form(..))


type alias Builder = B.Builder ()


type alias Set = B.Set ()


none : Builder
none = B.none


root : Set -> Builder
root = B.root


float : Axis -> Float -> Builder
float axis current = B.float axis current <| always ()


int : { min: Int, max : Int, step : Int } -> Int -> Builder
int axis current = B.int axis current <| always ()


number : Axis -> Float -> Builder
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> Builder
xy xAxis yAxis = B.xy xAxis yAxis <| always ()


coord : ( Axis, Axis ) -> ( Float, Float ) -> Builder
coord = xy


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Builder
input toString fromString current = B.input toString fromString current <| always ()


text : String -> Builder
text default = B.text default <| always ()


color : Color -> Builder
color current = B.color current <| always ()


button : Builder
button = B.button <| always ()


buttonWith : Icon -> Builder
buttonWith icon_ = B.buttonWith icon_ <| always ()


toggle : Bool -> Builder
toggle current = B.toggle current <| always ()


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
    B.choice pShape cShape toLabel items current compare <| always ()


choiceIcons
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceIcons pShape cShape toLabel items current compare =
    B.choiceIcons pShape cShape toLabel items current compare <| always ()


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
choiceAuto pShape cShape toLabel items current =
    B.choiceAuto pShape cShape toLabel items current <| always ()



strings
     : List String
    -> String
    -> Builder
strings options current =
    B.strings options current <| always ()


labels
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
labels toLabel items current compare =
    B.labels toLabel items current compare <| always ()


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
labelsAuto toLabel items current =
    B.labelsAuto toLabel items current <| always ()


palette
     : PanelShape
    -> List Color
    -> Color
    -> Builder
palette shape options current =
    B.palette shape options current <| always ()


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
