module Tron.Control.GenUI.Nest exposing (groupTo, groupFrom, choiceTo, choiceFrom, loadPanelShape, root)


import GenUI

import Array exposing (Array)

import Tron.Tree.Internals exposing (NestShape)
import Tron.Control as Core
import Tron.Control.Impl.Nest as Nest exposing (GroupControl, ChoiceControl)
import Tron.Control.GenUI.Button as Button
import Tron.Style.PanelShape as PS exposing (PanelShape(..), Pagination(..))


convertPanelShape : PanelShape -> GenUI.Pages
convertPanelShape pshape =
    if PS.pagesEnabled pshape then
        case PS.numify pshape of
            ( cols, rows ) ->
                GenUI.Distribute { maxInRow = cols, maxInColumn = rows }
    else GenUI.Single



groupTo : PanelShape -> (item -> GenUI.Property x) -> GroupControl item a -> GenUI.Def x
groupTo pshape toProp (Core.Control items { form, face, page } _) =
    GenUI.Nest
        { children = Array.toList <| Array.map toProp items
        , panel =
            { form = case Debug.log "group form (to)" form of
                Nest.Expanded -> GenUI.Expanded
                _ -> GenUI.Collapsed
            , button = Maybe.withDefault GenUI.ExpandCollapse <| Maybe.map Button.faceTo face
            , allOf = Nothing
            , page = GenUI.Page page
            , pages = convertPanelShape pshape
            }
        , nestAt = Nothing
        }


groupFrom : (GenUI.Property x -> Maybe item) -> GenUI.Def x -> Result (List (GenUI.Property x)) (GroupControl item ())
groupFrom toItem def =
    case def of
        GenUI.Nest nestDef ->
            nestDef.children
                |> adaptItems toItem
                |> Result.map
                    (\items ->
                        Core.Control
                            (Array.fromList items)
                            { form =
                                case Debug.log "group form (from)" <| nestDef.panel.form of
                                    GenUI.Expanded -> Nest.Expanded
                                    GenUI.Collapsed -> Nest.Collapsed
                            , face = Just <| Button.faceFrom nestDef.panel.button
                            , page = adaptPage nestDef.panel.page
                            }
                            ()
                    )
        _ -> Err []




choiceTo : PanelShape -> (item -> Maybe GenUI.SelectItem) -> ChoiceControl item a -> GenUI.Def x
choiceTo pshape toSelectItem (Core.Control items { form, face, mode, selected, page } _) =
    case adaptItems toSelectItem <| Array.toList items of
        Ok values ->
            GenUI.Select
                { values = values
                , nestAt = Nothing
                , current =
                    items
                        |> Array.get selected
                        |> Maybe.andThen (toSelectItem >> Maybe.map .value)
                        |> Maybe.withDefault ""
                , kind =
                     case mode of
                        Nest.Pages ->
                            GenUI.Choice
                                { form = case Debug.log "choice form (to)" <| form of
                                    Nest.Expanded -> GenUI.Expanded
                                    Nest.Collapsed -> GenUI.Collapsed
                                    Nest.Detached -> GenUI.Collapsed
                                , button = Maybe.withDefault GenUI.Focus <| Maybe.map Button.faceTo face
                                , allOf = Nothing
                                , page = GenUI.Page page
                                , pages = convertPanelShape pshape
                                }
                        Nest.Knob ->
                            GenUI.Knob
                        Nest.SwitchThrough ->
                            GenUI.Switch
                }
        Err _ -> GenUI.Ghost -- FIXME


choiceFrom : (item -> String -> Bool) -> (GenUI.SelectItem -> Maybe item) -> GenUI.Def x -> Result (List GenUI.SelectItem) (ChoiceControl item ())
choiceFrom compare toItem def =
    case def of
        GenUI.Select selectDef ->
            adaptItems toItem selectDef.values
                    |> Result.map
                        (\passedItems ->
                            Core.Control
                                (Array.fromList passedItems)
                                { form = case selectDef.kind of
                                    GenUI.Choice { form } ->
                                        case Debug.log "choice form (from)" form of
                                            GenUI.Expanded -> Nest.Expanded
                                            GenUI.Collapsed -> Nest.Collapsed
                                    GenUI.Knob -> Nest.Expanded
                                    GenUI.Switch -> Nest.Expanded
                                , face =
                                    case selectDef.kind of
                                        GenUI.Choice panel -> Just <| Button.faceFrom panel.button
                                        GenUI.Knob -> Nothing
                                        GenUI.Switch -> Nothing
                                , mode =
                                    case selectDef.kind of
                                        GenUI.Choice _ -> Nest.Pages
                                        GenUI.Knob -> Nest.Knob
                                        GenUI.Switch -> Nest.SwitchThrough
                                , prevSelected = Nothing
                                , selected =
                                    passedItems
                                        |> List.indexedMap Tuple.pair
                                        |> List.foldl (\(index, value) prev -> if compare value selectDef.current then index else prev) 0
                                , page =
                                    case selectDef.kind of
                                        GenUI.Choice { page } ->
                                            adaptPage page
                                        GenUI.Knob -> 0
                                        GenUI.Switch -> 0
                                }
                                ()
                        )
        _ -> Err []


adaptPage : GenUI.Page -> Int
adaptPage page = -- FIXME
    case Debug.log "adaptPage" page of -- FIXME
        GenUI.First -> 0
        GenUI.Last -> -1
        GenUI.ByCurrent -> -1
        GenUI.Page n -> n

{- choiceFrom : (GenUI.SelectItem -> Maybe String) -> GenUI.Def x -> Result (List GenUI.SelectItem) (ChoiceControl String ())
choiceFrom = choiceFrom_ (==) -}



{- nestShapeFrom : ( Maybe GenUI.CellShape, GenUI.NestShape ) -> NestShape
nestShapeFrom ( maybeCellShape, selectDef ) =
    (
    , maybeCellShape |> Maybe.withDefault { rows = }

    ) -}


adaptPages : GenUI.Pages -> PanelShape
adaptPages p =
    case Debug.log "adaptPages" p of
        GenUI.Auto -> PS.auto
        GenUI.Single ->  PS.auto |> PS.singlePage
        GenUI.Distribute { maxInRow, maxInColumn } -> PS.create ( maxInRow, maxInColumn )
        GenUI.Exact n -> PS.auto -- FIXME


loadPanelShape : GenUI.Def x -> Maybe PanelShape
loadPanelShape def =
    case def of
        GenUI.Select selectDef ->
             case selectDef.kind of
                GenUI.Choice { pages } ->
                    adaptPages pages |> Just
                _ -> Nothing
        GenUI.Nest { panel } ->
            adaptPages panel.pages |> Just
        _ -> Nothing


adaptItems : (a -> Maybe b) -> List a -> Result (List a) (List b)
adaptItems toItem items =
    let
        maybeItems =
            List.map (\prop -> ( prop, toItem prop )) items
        failedProps =
            List.filterMap
                (\(prop, maybeItem) ->
                    case maybeItem of
                        Nothing -> Just prop
                        Just _ -> Nothing
                )
                maybeItems
        passedItems =
            List.filterMap identity
                <| List.map Tuple.second
                <| maybeItems
    in
        if List.isEmpty failedProps then
            Ok passedItems
        else Err failedProps


root : List item -> a -> (GroupControl item a)
root items a =
    Core.Control
        (Array.fromList items)
        { form = Nest.Expanded
        , face = Nothing
        , page = 0
        }
        a