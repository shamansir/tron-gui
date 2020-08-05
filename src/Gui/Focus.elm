module Gui.Focus exposing (..)


import Gui.Def exposing (..)
import Gui.Nest exposing (..)


type Focus = Focus NestPos


maxPos = 1000 -- FIXME: do not expost


none : Focus
none = Focus nowhere


fromList : List Int -> Focus
fromList = NestPos >> Focus


toList : Focus -> List Int
toList (Focus (NestPos list)) = list


at : Int -> Focus
at = List.singleton >> fromList


type Direction
    = Up -- if there is a nesting, move to the first item of its items; else, do nothing
    | Down -- if there is a nesting, move to the parent of this nesting; else, do nothing
    | Right -- move focus right; if it's not initialized, select the first item in this nesting
    | Left -- move focus left; if it's not initialized, select the last item in this nesting



on : Focus -> Nest umsg -> Nest umsg
on focus nest =
    let
        focusList = focus |> toList
        alignIndex cells index =
            if index >= List.length cells
                then List.length cells - 1
                else if index < -1 then -1 else index
        setFocus focusLeft innerNest =
            case focusLeft of
                head :: tail ->
                    let
                        alignedIndex = alignIndex innerNest.cells head
                    in
                        if alignedIndex >= 0 then
                            { innerNest
                            | focus = alignedIndex
                            , cells =
                                innerNest.cells |>
                                    List.indexedMap
                                        (\cellIndex cell ->
                                            if cellIndex == alignedIndex then
                                                case cell of
                                                    (Nested label expand innerNest_) ->
                                                        Nested label expand
                                                            <| setFocus tail innerNest_
                                                    (Choice label expand item handler innerNest_) ->
                                                        Choice label expand item handler
                                                            <| setFocus tail innerNest_
                                                    _ -> cell
                                            else cell
                                        )
                            }
                        else innerNest |> reset
                [] ->
                    innerNest |> reset
    in
        setFocus focusList nest



reset : Nest umsg -> Nest umsg
reset =
    traverseAllNests
        (\innerNest _ ->
            { innerNest
            | focus = -1
            }
        )


get : Nest umsg -> Focus
get nest =
    let
        (Focus innerFocus) = nest |>
            foldCells (\cell pos prevFocus ->
                case cell of
                    Nested _ Expanded { focus } ->
                        let focusPos = pos |> deeper focus
                        in if isDeeper focusPos prevFocus
                            then focusPos
                            else prevFocus
                    Choice _ Expanded _ _ { focus } ->
                        let focusPos = pos |> deeper focus
                        in if isDeeper focusPos prevFocus
                            then focusPos
                            else prevFocus
                    _ -> prevFocus
            ) nowhere |> Focus
    in
        if isSamePos innerFocus nowhere then
            nowhere |> deeper nest.focus |> Focus
        else Focus innerFocus


shift : Direction -> Focus -> Focus
shift dir focus =
    case dir of
        Up -> fromList <| 0 :: (focus |> toList)
        Down -> focus |> toList |> List.tail |> Maybe.withDefault [] |> fromList
        Right ->
            let
                focusList = focus |> toList
                maybeTop = focusList |> List.head
                maybeOthers = focusList |> List.tail
            in
                case maybeTop of
                    Just topIndex ->
                        if (topIndex > 0) then
                            (topIndex - 1) :: (maybeOthers |> Maybe.withDefault []) |> fromList
                        else
                            0 :: (maybeOthers |> Maybe.withDefault []) |> fromList
                    Nothing ->
                        at 0
        Left ->
            let
                focusList = focus |> toList
                maybeTop = focusList |> List.head
                maybeOthers = focusList |> List.tail
            in
                case maybeTop of
                    Just topIndex ->
                        (topIndex + 1) :: (maybeOthers |> Maybe.withDefault []) |> fromList
                    Nothing ->
                        at maxPos -- uses maximum value to align it to the latest cell in a row later
