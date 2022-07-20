module Tron.Control.GenUI.Nest exposing (groupTo, groupFrom, choiceTo, choiceFrom)


import GenUI

import Array exposing (Array)

import Tron.Control as Core
import Tron.Control.Impl.Nest as Nest exposing (GroupControl, ChoiceControl)
import Tron.Control.GenUI.Button as Button


groupTo : (item -> GenUI.Property) -> GroupControl item a -> GenUI.Def
groupTo toProp (Core.Control items { form, face, page } _) =
    GenUI.Nest
        { children = Array.toList <| Array.map toProp items
        , form =
            case form of
                Nest.Expanded -> GenUI.Expanded
                _ -> GenUI.Collapsed
        , nestAt = Nothing
        , face = Maybe.withDefault GenUI.Default <| Maybe.map Button.faceTo face
        , shape = { cols = -1, rows = -1, pages = 1 } -- FIXME
        , page = page
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
                            { form =
                                case nestDef.form of
                                    GenUI.Expanded -> Nest.Expanded
                                    GenUI.Collapsed -> Nest.Collapsed
                            , face = Just <| Button.faceFrom nestDef.face
                            , page = nestDef.page
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
                                { face = Maybe.withDefault GenUI.Default <| Maybe.map Button.faceTo face
                                , form = case form of
                                    Nest.Expanded -> GenUI.Expanded
                                    Nest.Collapsed -> GenUI.Collapsed
                                    Nest.Detached -> GenUI.Collapsed
                                , page = 1 -- FIXME
                                , shape = { cols = -1, rows = -1, pages = 1 } -- FIXME
                                }
                        Nest.Knob ->
                            GenUI.Knob
                        Nest.SwitchThrough ->
                            GenUI.Switch
                }
        Err _ -> GenUI.Ghost -- FIXME


choiceFrom_ : (item -> String -> Bool) -> (GenUI.SelectItem -> Maybe item) -> GenUI.Def -> Result (List GenUI.SelectItem) (ChoiceControl item ())
choiceFrom_ compare toItem def =
    case def of
        GenUI.Select selectDef ->
            adaptItems toItem selectDef.values
                    |> Result.map
                        (\passedItems ->
                            Core.Control
                                (Array.fromList passedItems)
                                { form = case selectDef.kind of
                                    GenUI.Choice { form } ->
                                        case form of
                                            GenUI.Expanded -> Nest.Expanded
                                            GenUI.Collapsed -> Nest.Collapsed
                                    GenUI.Knob -> Nest.Expanded
                                    GenUI.Switch -> Nest.Expanded
                                , face =
                                    case selectDef.kind of
                                        GenUI.Choice { face } -> Just <| Button.faceFrom face
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
                                        GenUI.Choice { page } -> page
                                        GenUI.Knob -> 0
                                        GenUI.Switch -> 0
                                }
                                ()
                        )
        _ -> Err []


choiceFrom : (GenUI.SelectItem -> Maybe String) -> GenUI.Def -> Result (List GenUI.SelectItem) (ChoiceControl String ())
choiceFrom = choiceFrom_ (==)



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
