module Tron.Property.Controls exposing (..)


import Array exposing (Array)
import Array.Extra.Zipper as Z exposing (zip, Zipper(..))

import Tron.Property exposing (Property(..), NestShape, foldP, updateAt, move)
import Tron.Path as Path exposing (Path)
import Tron.Style.CellShape as CS exposing (CellShape)
import Tron.Style.PanelShape as PS exposing (PanelShape)

import Tron.Pages as Pages

import Tron.Control.Impl.Button as Button exposing (..)
import Tron.Control.Impl.Number as Number exposing (..)
import Tron.Control.Impl.XY as XY exposing (..)
import Tron.Control.Impl.Text as Text exposing (..)
import Tron.Control.Impl.Color as Color exposing (..)
import Tron.Control.Impl.Toggle as Toggle exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (..)


defaultNestShape : NestShape
defaultNestShape = ( PS.auto, CS.single )


transferTransientState : Property a -> Property a -> Property a
transferTransientState propA propB =
    let
        f zipper =
            case zipper of
                -- FIXME: add `Control.hasTransientState` or smth like that
                Z.Both (Choice focusA _ controlA) (Choice _ shapeB controlB) ->
                    Choice focusA shapeB
                        (Nest.getTransientState controlA
                            |> Nest.restoreTransientState controlB)
                Z.Both (Group focusA _ controlA) (Group _ shapeB controlB) ->
                    Group focusA shapeB
                        (Nest.getTransientState controlA
                            |> Nest.restoreTransientState controlB)
                Z.Both (Text controlA) (Text controlB) ->
                    Text
                        (Text.getTransientState controlA
                            |> Text.restoreTransientState controlB)
                Z.Both (Live innerPropA) (Live innerPropB) ->
                    Live <| transferTransientState innerPropA innerPropB
                Z.Both _ propB_ -> propB_
                Z.Left propB_ -> propB_
                Z.Right propA_ -> propA_ -- FIXME: `Nil`?
    in move f propA propB


-- TODO: better use the functions below directly from their controls


finishEditingAt : Path -> Property a -> Property a
finishEditingAt path =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text <| Text.finishEditing control
                _ -> prop


updateTextAt : Path -> String -> Property a -> Property a
updateTextAt path newValue =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text ( control |> Text.updateText newValue )
                _ -> prop



-- updateAndExecute : (v -> v) -> Control s v a -> ( Control s v a, a )

ensureEditing : Property a -> Property a
ensureEditing prop =
    case prop of
        Text control ->
            Text <| Text.ensureEditing control
        _ -> prop


expand : Property a -> Property a
expand prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.expand control
        Choice focus shape control ->
            Choice focus shape <| Nest.expand control
        _ -> prop


collapse : Property a -> Property a
collapse prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.collapse control
        Choice focus shape control ->
            Choice focus shape <| Nest.collapse control
        _ -> prop


isExpanded : Property a -> Maybe Nest.Form
isExpanded prop =
    case prop of
        Group _ _ control ->
            Just <| Nest.getForm control
        Choice _ _ control ->
            Just <| Nest.getForm control
        _ -> Nothing


detach : Property a -> Property a
detach prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.detach control
        Choice focus shape control ->
            Choice focus shape <| Nest.detach control
        _ -> prop


switchPage : Pages.PageNum -> Property a -> Property a
switchPage pageNum prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.switchTo pageNum <| control
        Choice focus shape control ->
            Choice focus shape <| Nest.switchTo pageNum <| control
        _ -> prop


expandAt : Path -> Property a -> Property a
expandAt path =
    updateAt path expand


detachAt : Path -> Property a -> Property a
detachAt path =
    updateAt path detach


switchPageAt : Path -> Pages.PageNum -> Property a -> Property a
switchPageAt path pageNum =
    updateAt path <| switchPage pageNum


detachAll : Property a -> Property a
detachAll =
    foldP <| always detach


toggle : Property a -> Property a
toggle prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggle control
        _ -> prop


toggleAt : Path -> Property a -> Property a
toggleAt path =
    updateAt path toggle


toggleOn : Property a -> Property a
toggleOn prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggleOn control
        _ -> prop


toggleOff : Property a -> Property a
toggleOff prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggleOff control
        _ -> prop


ensureEditingAt : Path -> Property a -> Property a
ensureEditingAt path =
    updateAt path ensureEditing


setChoiceMode : Nest.ChoiceMode -> Property a -> Property a
setChoiceMode newMode prop =
    case prop of
        Choice focus shape control ->
            Choice focus shape
                <| Nest.setChoiceMode newMode
                <| control
        _ -> prop


{-
reshape : Shape -> Property a -> Property a
reshape shape prop =
    case prop of
        Group ( Control ( _, items ) ( expanded, focus ) handler ) ->
            Group ( Control ( shape, items ) ( expanded, focus ) handler )
        _ -> prop
-}


isGhost : Property a -> Bool
isGhost prop =
    case prop of
        Nil _ -> True
        _ -> False


noGhosts : List (Property a) -> List (Property a)
noGhosts = List.filter (not << isGhost)


getCellShape : Property a -> Maybe CellShape
getCellShape prop =
    case prop of
        Choice _ ( _, cellShape ) _ ->
            Just cellShape
        Group _ ( _, cellShape ) _ ->
            Just cellShape
        Live innerProp ->
            getCellShape innerProp
        _ -> Nothing


getPageNum : Property a -> Maybe Pages.PageNum
getPageNum prop =
    case prop of
        Choice _ _ control ->
            Just <| Nest.getPage control
        Group _ _ control ->
            Just <| Nest.getPage control
        Live innerProp ->
            getPageNum innerProp
        _ -> Nothing


getItems : Property a -> Maybe (Array (Path.Label, Property a))
getItems prop =
    case prop of
        Choice _ _ control ->
            Just <| Nest.getItems control
        Group _ _ control ->
            Just <| Nest.getItems control
        Live innerProp ->
            getItems innerProp
        _ -> Nothing


getSelected : Property a -> Maybe ( Path.Label, Property a )
getSelected prop =
    case prop of
        Choice _ _ control ->
            control |> Nest.getSelected
        _ -> Nothing


isSelected : Property a -> Int -> Bool
isSelected prop index =
    case prop of
        Choice _ _ control ->
            Nest.isSelected control index
        _ -> False


setFace : Button.Face -> Property a -> Property a
setFace face prop =
    case prop of
        Action control ->
            Action
                <| Button.setFace face
                <| control
        Group focus shape control ->
            Group focus shape
                <| Nest.setFace face
                <| control
        Choice focus shape control ->
            Choice focus shape
                <| Nest.setFace face
                <| control
        Live innerProp ->
            Live
                <| setFace face
                <| innerProp
        _ -> prop


clearFace : Property a -> Property a
clearFace prop =
    case prop of
        Action control ->
            Action
                <| Button.setFace Button.Default
                <| control
        Group focus shape control ->
            Group focus shape
                <| Nest.clearFace
                <| control
        Choice focus shape control ->
            Choice focus shape
                <| Nest.clearFace
                <| control
        Live innerProp ->
            Live
                <| clearFace
                <| innerProp
        _ -> prop


toChoice : Property a -> Property a
toChoice prop =
    case prop of
        Group focus shape control ->
            Choice focus shape
                --<| Control.mapByValue (.selected >> f)
                <| Nest.toChoice
                <| control
        _ -> prop


setPanelShape : PanelShape -> Property a -> Property a
setPanelShape ps prop =
    case prop of
        Group focus ( _, cs ) control ->
            Group focus ( ps, cs ) control
        Choice focus ( _, cs ) control ->
            Choice focus ( ps, cs ) control
        _ -> prop


setCellShape : CellShape -> Property a -> Property a
setCellShape cs prop =
    case prop of
        Group focus ( ps, _ ) control ->
            Group focus ( ps, cs ) control
        Choice focus ( ps, _ ) control ->
            Choice focus ( ps, cs ) control
        _ -> prop



updatePanelShape : (PanelShape -> PanelShape) -> Property a -> Property a
updatePanelShape fn prop =
    case prop of
        Group focus ( ps, cs ) control ->
            Group focus ( fn ps, cs ) control
        Choice focus ( ps, cs ) control ->
            Choice focus ( fn ps, cs ) control
        _ -> prop


updateCellShape : (CellShape -> CellShape) -> Property a -> Property a
updateCellShape fn prop =
    case prop of
        Group focus ( ps, cs ) control ->
            Group focus ( ps, fn cs ) control
        Choice focus ( ps, cs ) control ->
            Choice focus ( ps, fn cs ) control
        _ -> prop


togglePagination : Property a -> Property a
togglePagination = updatePanelShape PS.togglePagination


append : ( Path.Label, Property a ) -> Property a -> Property a
append ( label, prop ) toProp =
    case toProp of
        Choice focus shape control ->
            Choice focus shape
                (control |> Nest.append ( label, prop ))
        Group focus shape control ->
            Group focus shape
                (control |> Nest.append ( label, prop ))
        _ -> toProp


remove : ItemId -> Property a -> Property a
remove item fromProp =
    case fromProp of
        Choice focus shape control ->
            Choice focus shape
                (control |> Nest.remove item)
        Group focus shape control ->
            Group focus shape
                (control |> Nest.remove item)
        _ -> fromProp


forward : ItemId -> Property a -> Property a
forward item inProp =
    case inProp of
        Choice focus shape control ->
            Choice focus shape
                (control |> Nest.forward item)
        Group focus shape control ->
            Group focus shape
                (control |> Nest.forward item)
        _ -> inProp


backward : ItemId -> Property a -> Property a
backward item inProp =
    case inProp of
        Choice focus shape control ->
            Choice focus shape
                (control |> Nest.backward item)
        Group focus shape control ->
            Group focus shape
                (control |> Nest.backward item)
        _ -> inProp
