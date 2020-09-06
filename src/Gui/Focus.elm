module Gui.Focus exposing (..)


import Gui.Control exposing (..)
import Gui.Over exposing (..)

import Array


type Direction
    = Up
    | Down
    | Right
    | Left


on : Path -> Over msg -> Over msg
on (Path path) root =
    case ( path, root ) of
        ( [], _ ) -> root
        ( x::xs, Group (Control ( shape, items ) ( expanded, _ ) handler) ) ->
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
                    ( expanded
                    , Just <| Focus normalizedX
                    )
                    handler
                )
        ( _, _ ) -> root


find : Over msg -> Path
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


shift : Direction -> Over msg -> Over msg
shift direction root =
    let
        (Path currentFocus) = find root
        curFocusArr = currentFocus |> Array.fromList
        indexOfLast = Array.length curFocusArr - 1
        nextFocus =
            Path <| Array.toList <|
                case direction of
                    Up ->
                        curFocusArr |> Array.push 0
                    Down ->
                        curFocusArr |> Array.slice 0 -1
                    Left ->
                        curFocusArr
                            |> Array.indexedMap
                                (\idx item ->
                                    if idx /= indexOfLast then item
                                    else item - 1
                                )
                    Right ->
                        curFocusArr
                            |> Array.indexedMap
                                (\idx item ->
                                    if idx /= indexOfLast then item
                                    else item + 1
                                )
    in root |> on nextFocus
