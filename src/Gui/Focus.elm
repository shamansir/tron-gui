module Gui.Focus exposing (..)


import Gui.Control exposing (..)
import Gui.Property exposing (..)

import Array


type Focused
    = FocusedBy Int
    | NotFocused


type Direction
    = Up
    | Down
    | Right
    | Left


clear : Property msg -> Property msg
clear =
    mapReplace
        (\_ prop ->
            case prop of
                Group (Control setup ( expanded, _ ) handler) ->
                    Group (Control setup ( expanded, Nothing ) handler)
                _ -> prop
        )



on : Path -> Property msg -> Property msg
on (Path path) root =
    case ( path, root ) of
        ( [], _ ) -> root
        ( x::xs, Group (Control ( shape, items ) ( Expanded, _ ) handler) ) ->
            let
                itemsCount = Array.length items
                normalizedX =
                    if x < 0 then 0
                    else if x >= itemsCount then itemsCount - 1
                    else x

            in
            Group
                (Control
                    ( shape
                    ,
                        items |>
                            Array.indexedMap
                            (\index ( label, innerItem ) ->
                                ( label
                                ,
                                    if index == normalizedX then
                                        on (Path xs) innerItem
                                    else innerItem
                                )
                            )

                    )
                    ( Expanded
                    , Just <| Focus normalizedX
                    )
                    handler
                )
        ( _, _ ) -> root


find : Property msg -> Path
find root =
    let
        helper control =
            case control of
                Group (Control ( _, items ) ( _, focus ) handler) ->
                    focus
                        |> Maybe.andThen
                            (\(Focus theFocus) ->
                                items
                                    |> Array.get theFocus
                                    |> Maybe.map (Tuple.pair theFocus)
                            )
                        |> Maybe.map
                            (\( theFocus, ( _, focusedItem ) ) ->
                                theFocus :: helper focusedItem
                            )
                        |> Maybe.withDefault []
                _ -> []
    in Path <| helper root


shift : Direction -> Property msg -> Property msg
shift direction root =
    let
        (Path currentFocus) = find root
        curFocusArr = currentFocus |> Array.fromList
        indexOfLast = Array.length curFocusArr - 1
        focusedOnSmth = Array.length curFocusArr > 0
        nextFocus =
            Path <| Array.toList <|
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
    in root |> clear |> on nextFocus


focused : Property msg -> Path -> Focused
focused root (Path path) =
    let
        helper iPath flevel prop =
            case ( iPath, prop ) of
                ( [], _ ) ->
                    if flevel < 0
                        then NotFocused
                        else FocusedBy flevel
                ( x::xs, Group (Control ( _, items ) ( _, Just (Focus focus) ) handler) ) ->
                    if focus == x then
                        case items |> Array.get focus  of
                            Just ( _, item ) -> helper xs (flevel + 1) item
                            Nothing -> NotFocused
                    else NotFocused
                _ -> NotFocused
    in
        helper path -1 root
