module Tron.Tree.Expose.GenUI exposing (to, from)


import GenUI exposing (GenUI)

-- import Tron.Tree as Tree
import Tron.Control as Control
import Tron.Tree.Internals as Tree exposing (..)
import Tron.Path as Path
import Tron.Style.PanelShape as PS exposing (PanelShape)
import Tron.Style.CellShape as CS exposing (CellShape)

import Tron.Control.GenUI.Button as Button
import Tron.Control.GenUI.Number as Number
import Tron.Control.GenUI.XY as XY
import Tron.Control.GenUI.Text as Text
import Tron.Control.GenUI.Toggle as Toggle
import Tron.Control.GenUI.Color as Color
import Tron.Control.GenUI.Nest as Nest

import Tron.Control.Impl.Button as B

import GenUI


treeToGenUIAt : Path.Label -> Tree a -> GenUI.Property a
treeToGenUIAt label t =
    let
        makeProp =
            makeProp_ Nothing
        makePProp shape =
            makeProp_ <| Just shape
        makeProp_ maybeShape name def =
            (
                { name = name
                , shape =
                    maybeShape
                        |> Maybe.map CS.numify
                        |> Maybe.map (\(rows, cols) -> { rows = floor rows, cols = floor cols })
                , def = def
                , live = False
                , property = Nothing
                }
            , get t
            )
    in case t of
        Nil a -> GenUI.root a -- FIXME
        Action button -> makeProp label <| Button.to button
        Number number -> makeProp label <| Number.to number
        Coordinate coord -> makeProp label <| XY.to coord
        Text text -> makeProp label <| Text.to text
        Toggle toggle -> makeProp label <| Toggle.to toggle
        Color color -> makeProp label <| Color.to color
        Choice _ ( panelShape, cellShape ) control ->
            makePProp cellShape label
                <| Nest.choiceTo panelShape
                    (\(valueLabel, innerTree) ->
                        case innerTree of
                            Action button ->
                                Button.toSelectItem valueLabel button
                                    |> Just
                            _ -> Nothing
                    )
                    control
        Group _ ( panelShape, cellShape ) control ->
            makePProp cellShape label
                <| Nest.groupTo panelShape
                    (\(innerLabel, innerTree) -> treeToGenUIAt innerLabel innerTree)
                    control
        Live tree ->
            let
                ( prop, a ) = treeToGenUIAt label tree
            in
                (
                    { prop
                    | live = True
                    }, a
                )


treeToGenUI : Tree a -> GenUI.Property a
treeToGenUI =
   treeToGenUIAt "root"


genUIToTree : GenUI.Property a -> Result String (Tree (Maybe a))
genUIToTree ( prop, a ) =
    let
        setToA = Tree.map <| always <| Just a
    in
    ( case prop.def of
        GenUI.Ghost -> Ok <| Nil <| Just a
        GenUI.NumInt _ -> Result.map setToA <| Result.fromMaybe "not a number prop" <| Maybe.map Number <| Number.from prop.def
        GenUI.NumFloat _ -> Result.map setToA <| Result.fromMaybe "not a number prop" <| Maybe.map Number <| Number.from prop.def
        GenUI.XY _ -> Result.map setToA <| Result.fromMaybe "not a XY prop" <| Maybe.map Coordinate <| XY.from prop.def
        GenUI.Toggle _ -> Result.map setToA <| Result.fromMaybe "not a toggle prop" <| Maybe.map Toggle <| Toggle.from prop.def
        GenUI.Color _ -> Result.map setToA <| Result.fromMaybe "not a color prop" <| Maybe.map Color <| Color.from prop.def
        GenUI.Textual _ -> Result.map setToA <| Result.fromMaybe "not a text prop" <| Maybe.map Text <| Text.from prop.def
        GenUI.Action _ -> Result.map setToA <| Result.fromMaybe "not an action prop" <| Maybe.map Action <| Button.from prop.def
        GenUI.Select _ -> Result.mapError (always "not a select prop")
                                -- <| Result.map ()
                                <| Result.map (Choice Nothing ( Nest.loadPanelShape prop.def |> Maybe.withDefault PS.auto, {- FIXME: prop.shape |> Maybe.withDefault -} CS.default ))
                                <| Result.map (Control.map <| always <| Just a)
                                <| Nest.choiceFrom
                                        (\(label, _ ) value -> label == value)
                                        (\{ value, name, face } -> Just ( name |> Maybe.withDefault value, Action <| B.make Nothing <| Button.faceFrom face ))
                                        prop.def
        GenUI.Nest _ -> Result.map setToA
                                <| Result.mapError (always "not a select prop")
                                -- <| Result.map ()
                                <| Result.map (Group Nothing ( Nest.loadPanelShape prop.def |> Maybe.withDefault PS.auto, {- FIXME: prop.shape |> Maybe.withDefault -} CS.default ))
                                <| Result.map (Control.map <| always <| Just a)
                                <| Nest.groupFrom
                                        (\(prop_, a_) -> genUIToTree (prop_, a_) |> Result.toMaybe |> Maybe.map (Tuple.pair prop_.name))
                                        prop.def
        GenUI.Gradient _ -> Err "gradient props are not yet supported" -- FIXME
        GenUI.Progress _ -> Err "progress props are not yet supported" -- FIXME
        GenUI.Zoom _ -> Err "zoom props are not yet supported" -- FIXME
        -- TODO: live
    ) -- |> Result.map (Tree.map <| always a)


to : Tree a -> GenUI a
to t =
    { version = GenUI.version
    , root = [ treeToGenUI t ]
    }


from : GenUI a -> Result String (Tree a)
from _ = Err "failed" -- FIXME