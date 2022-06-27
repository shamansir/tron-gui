module Tron.Tree.Expose.GenUI exposing (to, from)


import GenUI exposing (GenUI)

import Tron.Tree.Internals as Tree exposing (..)
import Tron.Path as Path

import Tron.Control.GenUI.Button as Button
import Tron.Control.GenUI.Number as Number
import Tron.Control.GenUI.XY as XY
import Tron.Control.GenUI.Text as Text
import Tron.Control.GenUI.Toggle as Toggle
import Tron.Control.GenUI.Color as Color
import Tron.Control.GenUI.Nest as Nest


treeToGenUIAt : Path.Label -> Tree a -> GenUI.Property
treeToGenUIAt label t =
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
        Action button -> makeProp label <| Button.to button
        Number number -> makeProp label <| Number.to number
        Coordinate coord -> makeProp label <| XY.to coord
        Text text -> makeProp label <| Text.to text
        Toggle toggle -> makeProp label <| Toggle.to toggle
        Color color -> makeProp label <| Color.to color
        Choice focus ( panelShape, cellShape ) control ->
            makeProp label
                <| Nest.choiceTo
                    (\(valueLabel, innerTree) ->
                        case innerTree of
                            Action button ->
                                Button.toSelectItem valueLabel button
                                    |> Just
                            _ -> Nothing
                    )
                    control
        Group focus ( panelShape, cellShape ) control ->
            makeProp label
                <| Nest.groupTo
                    (\(innerLabel, innerTree) -> treeToGenUIAt innerLabel innerTree)
                    control
        Live tree ->
            let
                prop = treeToGenUIAt label tree
            in
                { prop
                | live = True
                }


treeToGenUI : Tree a -> GenUI.Property
treeToGenUI =
   treeToGenUIAt "root"


to : Tree a -> GenUI
to t =
    { version = GenUI.version
    , root = [ treeToGenUI t ]
    }


from : GenUI -> Result String (Tree ())
from _ = Err "failed"