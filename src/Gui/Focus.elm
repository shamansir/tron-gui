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
        ( x::xs, Group (Control config current handler) ) ->
            let
                itemsCount = Array.length config.items
                normalizedX =
                    if x < 0 then 0
                    else if x >= itemsCount then itemsCount - 1
                    else x

            in
            Group
                (Control
                    { config
                    | items = -- FIXME: items turn out to be a subject to change

                        config.items |>
                            Array.indexedMap
                            (\index ( label, innerItem ) ->
                                ( label
                                ,
                                    if index == normalizedX then
                                        on (Path xs) innerItem
                                    else innerItem
                                )
                            )

                    }
                    { current
                    | focus = Just <| Focus normalizedX
                    }
                    handler
                )
        ( _, _ ) -> root


find : Over msg -> Path
find root =
    let
        helper control =
            case control of
                Group (Control config current handler) ->
                    current.focus
                        |> Maybe.andThen
                            (\(Focus focus) ->
                                config.items
                                    |> Array.get focus
                                    |> Maybe.map (Tuple.pair focus)
                            )
                        |> Maybe.map
                            (\( focus, ( _, focusedItem ) ) ->
                                focus :: helper focusedItem
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
