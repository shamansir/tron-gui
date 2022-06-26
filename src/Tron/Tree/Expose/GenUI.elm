module Tron.Tree.Expose.GenUI exposing (to, from)


import GenUI exposing (GenUI)

import Tron.Tree.Internals as Tree exposing (..)

import Tron.Control.GenUI.Button as Button
import Tron.Control.GenUI.Number as Number
import Tron.Control.GenUI.XY as XY
import Tron.Control.GenUI.Text as Text
import Tron.Control.GenUI.Toggle as Toggle
import Tron.Control.GenUI.Color as Color
import Tron.Control.GenUI.Nest as Nest


treeToGenUI : Tree a -> GenUI.Property
treeToGenUI t =
    let
        makeProp name def =
            { name = name
            , shape = Nothing -- FIXME: where is the CellShape ?
            , def = def
            , live = False
            , property = Nothing
            }
    in case t of
        Nil _ -> GenUI.root -- FIXME
        Action button -> makeProp "button" <| Button.to button
        Number number -> makeProp "number" <| Number.to number
        Coordinate coord -> makeProp "xy" <| XY.to coord
        Text text -> makeProp "text" <| Text.to text
        Toggle toggle -> makeProp "toggle" <| Toggle.to toggle
        Color color -> makeProp "toggle" <| Color.to color
        Choice focus shape prop -> GenUI.root -- FIXME
        Group focus shape prop -> GenUI.root -- FIXME
        Live prop -> GenUI.root -- FIXME


to : Tree a -> GenUI
to t =
    { version = GenUI.version
    , root = [ treeToGenUI t ]
    }


from : GenUI -> Result String (Tree ())
from _ = Err "failed"