module Tron.FocusLogic exposing (..)


import Tron.Path as Path exposing (Path)
import Tron.Property exposing (..)
import Tron.Focus exposing (Focused(..), Level, Direction(..)) -- FIXME: better to have one module
import Tron.Control.Nest as Nest exposing (..)

import Array
import Array exposing (Array)


clear : Property a -> Property a
clear =
    replace
        (\_ prop ->
            case prop of
                Group focus shape control ->
                    Group Nothing shape control
                Choice _ shape control ->
                    Choice Nothing shape control
                _ -> prop
        )


on : Property a -> Path -> Property a
on root path =
    let

        goDeeper items x xs =
            let
                itemsCount = Array.length items
                normalizedX =
                    if x < 0 then 0
                    else if x >= itemsCount then itemsCount - 1
                    else x

            in
                ( items |>
                    Array.indexedMap
                    (\index ( label, innerItem ) ->
                        ( label
                        ,
                            if index == normalizedX then
                                on innerItem (Path.fromList xs)
                            else innerItem

                        )
                    )

                , FocusAt normalizedX
                )

    in
        case ( Path.toList path, root ) of -- TODO: navigate using labels?
            ( [], _ ) -> root
            ( (idx, _)::xs, Group curFocus shape control ) ->
                if control |> Nest.is Expanded then
                    case goDeeper (control |> Nest.getItems) idx xs of
                        ( nextItems, nextFocus ) ->
                            Group
                                (Just nextFocus)
                                shape
                                <| Nest.setItems nextItems <| control
                else Group curFocus shape control
            ( (idx, _)::xs, Choice curFocus shape control ) ->
                if control |> Nest.is Expanded then
                    case goDeeper (control |> Nest.getItems) idx xs of
                        ( nextItems, nextFocus ) ->
                            Choice
                                (Just nextFocus)
                                shape
                                <| Nest.setItems nextItems <| control
                else Choice curFocus shape control
            ( _, _ ) -> root


find : Property a -> Path
find root =
    let
        findDeeper : Maybe FocusAt -> Array ( Path.Label, Property a ) -> List ( Path.Index, Path.Label )
        findDeeper focus items =
            focus
                |> Maybe.andThen
                    (\(FocusAt theFocus) ->
                        items
                            |> Array.get theFocus
                            |> Maybe.map (Tuple.pair theFocus)
                    )
                |> Maybe.map
                    (\( theFocus, ( label, focusedItem ) ) ->
                        ( theFocus, label ) :: helper focusedItem
                    )
                |> Maybe.withDefault []
        helper prop =
            case prop of
                Group focus _ control ->
                    control |> Nest.getItems |> findDeeper focus
                Choice focus _ control ->
                    control |> Nest.getItems |> findDeeper focus
                _ -> []
    in Path.fromList <| helper root


shift : Direction -> Property a -> Property a
shift direction root =
    let
        currentFocus : Path
        currentFocus = find root
        maybeFocusedProp =
            root
                |> Tron.Property.find currentFocus
        nextItems =
            root
                |> Tron.Property.find currentFocus
                |> Maybe.andThen Tron.Property.getItems
                |> Maybe.withDefault Array.empty
        --curFocusArr = currentFocus |> Path.toList |> Array.fromList
        indexOfLast = Path.length currentFocus
        focusedOnSmth = Path.length currentFocus > 0
        -- FIXME: consider pages (move page when focus was changed)
        moveLastTo nextIndex =
            \idx (focusIdx, label) ->
                if idx /= indexOfLast
                    then (focusIdx, label)
                    else
                        case nextItems |> Array.get (nextIndex idx) of
                            Just (newLabel, _) ->
                                (nextIndex idx, newLabel)
                            Nothing -> (focusIdx, label)
        nextFocus =
            case direction of
                Up ->
                    case Array.get 0 nextItems of
                        Just ( label, _ ) ->
                            currentFocus |> Path.advance ( 0, label )
                        Nothing ->
                            currentFocus
                Down ->
                    currentFocus
                        |> Path.pop
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault currentFocus
                Left ->
                    if focusedOnSmth then
                        currentFocus
                            |> Path.update
                                (moveLastTo <| \idx -> idx - 1)
                    else
                        case nextItems |> Array.get 0  of
                            Just ( label, _ ) -> Path.start |> Path.advance ( 0, label )
                            Nothing -> Path.start
                Right ->
                    if focusedOnSmth then
                        currentFocus
                            |> Path.update
                                (moveLastTo <| \idx -> idx + 1)
                    else
                        case nextItems |> Array.get 0  of
                            Just ( label, _ ) -> Path.start |> Path.advance ( 0, label )
                            Nothing -> Path.start
    in on (clear root) nextFocus


focused : Property a -> Path -> Focused
focused root path =
    let
        helper iPath flevel prop =
            case ( iPath, prop ) of
                ( [], _ ) ->
                    if flevel < 0
                        then NotFocused
                        else FocusedBy flevel
                ( (idx, _)::xs, Group (Just (FocusAt focus)) _ control ) ->
                    if focus == idx then
                        case control |> Nest.get focus  of
                            Just ( _, item ) -> helper xs (flevel + 1) item
                            Nothing -> NotFocused
                    else NotFocused
                ( (idx, _)::xs, Choice (Just (FocusAt focus)) _ control ) ->
                    if focus == idx then
                        case control |> Nest.get focus of
                            Just ( _, item ) -> helper xs (flevel + 1) item
                            Nothing -> NotFocused
                    else NotFocused
                _ -> NotFocused
    in
        helper (Path.toList path) -1 root
