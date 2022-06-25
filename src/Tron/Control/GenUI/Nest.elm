module Tron.Control.GenUI.Nest exposing (groupTo, groupFrom, choiceTo, choiceFrom)


import GenUI

import Array exposing (Array)

import Tron.Control as Core
import Tron.Control.Impl.Nest as Nest exposing (GroupControl, ChoiceControl)
import Tron.Control.GenUI.Button as Button


groupTo : (item -> GenUI.Property) -> GroupControl item a -> GenUI.Def
groupTo toProp (Core.Control items { form, face } _) =
    GenUI.Nest
        { children = Array.toList <| Array.map toProp items
        , expand =
            case form of
                Nest.Expanded -> True
                _ -> False
        , nestAt = Nothing
        , face = Maybe.withDefault GenUI.Default <| Maybe.map Button.faceTo face
        , shape = { cols = -1, rows = -1, pages = 1 } -- FIXME
        }


groupFrom : (GenUI.Property -> Maybe item) -> GenUI.Def -> Result (List GenUI.Property) (GroupControl item ())
groupFrom toItem def =
    case def of
        GenUI.Nest nestDef ->
            nestDef.children
                |> adaptItems toItem
                |> Result.map
                    (\items ->
                        Core.Control
                            (Array.fromList items)
                            { form = if nestDef.expand then Nest.Expanded else Nest.Collapsed
                            , face = Just <| Button.faceFrom nestDef.face
                            , page = 0
                            }
                            ()
                    )
        _ -> Err []




choiceTo : (item -> Maybe GenUI.SelectItem) -> ChoiceControl item a -> GenUI.Def
choiceTo toSelectItem (Core.Control items { form, face, mode, selected } _) =
    case adaptItems toSelectItem <| Array.toList items of
        Ok values ->
            GenUI.Select
                { values = values
                -- , expand =
                --     case form of
                --         Nest.Expanded -> True
                --         _ -> False
                , nestAt = Nothing
                -- , face = Maybe.withDefault GenUI.Default <| Maybe.map Button.faceTo face
                , shape = { cols = -1, rows = -1, pages = 1 } -- FIXME
                , current = "" -- FIXME
                , kind =
                     case mode of
                        Nest.Pages ->
                            GenUI.Choice
                                { face = Maybe.withDefault GenUI.Default <| Maybe.map Button.faceTo face
                                , expand = case form of
                                    Nest.Expanded -> True
                                    _ -> False
                                }
                        Nest.Knob ->
                            GenUI.Knob
                        Nest.SwitchThrough ->
                            GenUI.Switch
                }
        Err _ -> GenUI.Root -- FIXME



choiceFrom : (GenUI.SelectItem -> Maybe item) -> GenUI.Def -> Result (List GenUI.SelectItem) (ChoiceControl item ())
choiceFrom toItem def =
    case def of
        GenUI.Select selectDef ->
            adaptItems toItem selectDef.values
                    |> Result.map
                        (\passedItems ->
                            Core.Control
                                (Array.fromList passedItems)
                                { form = Nest.Collapsed -- FIXME
                                , face = Just <| Button.faceFrom selectDef.face
                                , mode = Nest.Pages -- FIXME
                                , prevSelected = Nothing
                                , selected = 0 -- FIXME
                                , page = 0
                                }
                                ()
                        )
        _ -> Err []


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
