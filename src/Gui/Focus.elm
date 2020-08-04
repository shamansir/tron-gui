module Gui.Focus exposing (..)


import Gui.Def exposing (..)
import Gui.Nest exposing (..)


type Focus = Focus NestPos


none : Focus
none = Focus nowhere


shiftFocusBy : Int -> NestPos -> Nest umsg -> Nest umsg
shiftFocusBy amount position nest =
    let
        index = getIndexOf position |> Maybe.withDefault 0
        maybeParentPos = getParentPos position
        ensureFits { cells } =
            if ((index + amount) >= 0) && (index + amount < List.length cells) then
                index + amount
            else index
    in
        case maybeParentPos of
            Just parentPos ->
                nest |> traverseAllNests
                    (\deeperNest cellPosition ->
                        if (isSamePos cellPosition parentPos) then
                            { deeperNest
                            | focus = ensureFits deeperNest
                            }
                        else deeperNest
                    )
            Nothing ->
                { nest
                | focus = ensureFits nest
                }


shiftFocusTo : NestPos -> Nest umsg -> Nest umsg
shiftFocusTo position nest =
    let
        maybeParentPos = getParentPos position
        focusOn = getIndexOf position
                    |> Maybe.withDefault -1
    in
        case maybeParentPos of
            Just parentPos ->
                nest |> traverseAllNests
                    (\deeperNest cellPosition ->
                        if (isSamePos cellPosition parentPos) then
                            { deeperNest
                            | focus = focusOn
                            }
                        else deeperNest
                    )
            Nothing ->
                { nest
                | focus = focusOn
                }


findFocus: Nest umsg -> Focus
findFocus nest =
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
