module Tron.Tree.Values exposing (..)


import Array.Extra.Zipper as Z exposing (Zipper(..))


import Tron.Tree.Internals exposing (Tree(..), foldZipP, updateMany, move, insideOut)
import Tron.Tree.Paths exposing (pathifyWithValue)
import Tron.Control as Control
import Tron.Control.Impl.Nest as Nest exposing (..)
import Tron.Path exposing (Path)


valuesAreEqual : Tree a -> Tree b -> Bool
valuesAreEqual propA propB =
    case (propA, propB) of
        (Nil _, Nil _) -> True
        (Number controlA, Number controlB) ->
            Tuple.second (Control.getValue controlA) == Tuple.second (Control.getValue controlB)
        (Coordinate controlA, Coordinate controlB) ->
            Tuple.second (Control.getValue controlA) == Tuple.second (Control.getValue controlB)
        (Text controlA, Text controlB) ->
            Control.getValue controlA == Control.getValue controlB
        (Color controlA, Color controlB) ->
            Tuple.second (Control.getValue controlA) == Tuple.second (Control.getValue controlB)
        (Toggle controlA, Toggle controlB) ->
            Control.getValue controlA == Control.getValue controlB
        (Action _, Action _) -> True
        (Choice _ _ controlA, Choice _ _ controlB) ->
            Nest.whichSelected controlA == Nest.whichSelected controlB
        (Group _ _ _, Group _ _ _) -> True
        (Live innerPropA, Live innerPropB) -> valuesAreEqual innerPropA innerPropB
        (_, _) -> False



loadValueFrom : Tree a -> Tree a -> Tree a -- FIXME: return `Maybe`
loadValueFrom from to =
    case ( from, to ) of
        (Number controlA, Number controlB) ->
            Number <| Control.setValue (Control.getValue controlA) <| controlB
        (Coordinate controlA, Coordinate controlB) ->
            Coordinate <| Control.setValue (Control.getValue controlA) <| controlB
        (Text controlA, Text controlB) ->
            Text <| Control.setValue (Control.getValue controlA) <| controlB
        (Color controlA, Color controlB) ->
            Color <| Control.setValue (Control.getValue controlA) <| controlB
        (Toggle controlA, Toggle controlB) ->
            Toggle <| Control.setValue (Control.getValue controlA) <| controlB
        (Action controlA, Action controlB) ->
            Action <| Control.setValue (Control.getValue controlA) <| controlB
        (Choice _ _ controlA, Choice focus shape controlB) ->
            Choice focus shape <| Control.setValue (Control.getValue controlA) <| controlB
        (Group _ _ controlA, Group focus shape controlB) ->
            Group focus shape <| Control.setValue (Control.getValue controlA) <| controlB
        (Live _, Live _) -> to
            -- Number <| Control.setValue (Control.getValue controlB) <| controlA
        (_, _) -> to


changesBetween : Tree a -> Tree b -> List ( Path, Tree b )
changesBetween prev next =
    foldZipP
        (\zipper changes ->
            if not <| Z.run (always False) (always False) valuesAreEqual <| zipper then
                Z.getB zipper
                    |> Maybe.map (\prop -> prop :: changes)
                    |> Maybe.withDefault changes
            else changes
        )
        (prev |> pathifyWithValue)
        (next |> pathifyWithValue)
        []
    |> List.map insideOut


loadValues : Tree a -> Tree a -> Tree a
loadValues =
    move
        (Z.run
            identity -- FIXME: right should set `prop` to `Nil`?
            identity
            loadValueFrom
        )


loadChangedValues : Tree a -> Tree b -> Tree b -> Tree b
loadChangedValues prev next =
    updateMany <| changesBetween prev next


-- loadLiveValues : Property a -> Property b -> Property b
-- loadLiveValues = move
