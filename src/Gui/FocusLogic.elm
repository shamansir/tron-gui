module Gui.FocusLogic exposing (..)


import Gui.Path as Path exposing (Path)
import Gui.Property exposing (..)
import Gui.Focus exposing (Focused(..), Level, Direction(..)) -- FIXME: better to have one module
import Gui.Control.Nest as Nest exposing (..)

import Array
import Array exposing (Array)


clear : Property msg -> Property msg
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


on : Property msg -> Path -> Property msg
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
        case ( Path.toList path, root ) of
            ( [], _ ) -> root
            ( x::xs, Group curFocus shape control ) ->
                if control |> Nest.is Expanded then
                    case goDeeper (control |> Nest.getItems) x xs of
                        ( nextItems, nextFocus ) ->
                            Group
                                (Just nextFocus)
                                shape
                                <| Nest.setItems nextItems <| control
                else Group curFocus shape control
            ( x::xs, Choice curFocus shape control ) ->
                if control |> Nest.is Expanded then
                    case goDeeper (control |> Nest.getItems) x xs of
                        ( nextItems, nextFocus ) ->
                            Choice
                                (Just nextFocus)
                                shape
                                <| Nest.setItems nextItems <| control
                else Choice curFocus shape control
            ( _, _ ) -> root


find : Property msg -> Path
find root =
    let
        findDeeper : Maybe FocusAt -> Array ( a, Property msg ) -> List Int
        findDeeper focus items =
            focus
                |> Maybe.andThen
                    (\(FocusAt theFocus) ->
                        items
                            |> Array.get theFocus
                            |> Maybe.map (Tuple.pair theFocus)
                    )
                |> Maybe.map
                    (\( theFocus, ( _, focusedItem ) ) ->
                        theFocus :: helper focusedItem
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


shift : Direction -> Property msg -> Property msg
shift direction root =
    let
        currentFocus = find root
        curFocusArr = currentFocus |> Path.toList |> Array.fromList
        indexOfLast = Array.length curFocusArr - 1
        focusedOnSmth = Array.length curFocusArr > 0
        nextFocus =
            Path.fromList <| Array.toList <| -- FIXME: causes a lot of conversio
                case direction of
                    Up ->
                        curFocusArr |> Array.push 0
                    Down ->
                        curFocusArr |> Array.slice 0 -1
                    Left ->
                        if focusedOnSmth then
                            curFocusArr
                                |> Array.indexedMap
                                    (\idx item ->
                                        if idx /= indexOfLast then item
                                        else item - 1
                                    )
                        else Array.fromList [ 0 ] -- FIXME: causes a lot of conversions
                    Right ->
                        if focusedOnSmth then
                            curFocusArr
                                |> Array.indexedMap
                                    (\idx item ->
                                        if idx /= indexOfLast then item
                                        else item + 1
                                    )
                        else Array.fromList [ 0 ] -- FIXME: causes a lot of conversions
    in on (clear root) nextFocus


focused : Property msg -> Path -> Focused
focused root path =
    let
        helper iPath flevel prop =
            case ( iPath, prop ) of
                ( [], _ ) ->
                    if flevel < 0
                        then NotFocused
                        else FocusedBy flevel
                ( x::xs, Group (Just (FocusAt focus)) _ control ) ->
                    if focus == x then
                        case control |> Nest.get focus  of
                            Just ( _, item ) -> helper xs (flevel + 1) item
                            Nothing -> NotFocused
                    else NotFocused
                ( x::xs, Choice (Just (FocusAt focus)) _ control ) ->
                    if focus == x then
                        case control |> Nest.get focus of
                            Just ( _, item ) -> helper xs (flevel + 1) item
                            Nothing -> NotFocused
                    else NotFocused
                _ -> NotFocused
    in
        helper (Path.toList path) -1 root
