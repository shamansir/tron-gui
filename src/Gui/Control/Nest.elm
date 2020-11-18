module Gui.Control.Nest exposing (..)


import Array exposing (Array)

import Gui.Control as Core exposing (Control)

import Gui.Style.CellShape exposing (CellShape)


type alias Shape = ( Float, Float )


type NestState
    = Expanded
    | Collapsed
    | Detached


type SelectedAt = SelectedAt Int


-- TODO: move focus outside, only selection matters in the component for logic
-- TODO: may be even (Shape, CellShape) should also be outside

type alias GroupControl item msg =
    Core.Control
        ( ( Shape, CellShape ), Array item )
        ( NestState, () )
        msg


type alias ChoiceControl item msg =
    Core.Control
        ( ( Shape, CellShape ), Array item )
        ( NestState, SelectedAt )
        msg


get : Int -> Core.Control ( a, Array item ) value msg -> Maybe item
get n = getItems >> Array.get n


select : Int -> ChoiceControl item msg -> ChoiceControl item msg
select index (Core.Control setup ( expanded, _ ) handler) =
    Core.Control setup ( expanded, SelectedAt index ) handler


getSelected : ChoiceControl item msg -> Maybe item
getSelected control =
    get (whichSelected control) control


isSelected : ChoiceControl item msg -> Int -> Bool
isSelected control n = whichSelected control == n


whichSelected : ChoiceControl item msg -> Int
whichSelected (Core.Control _ ( _, SelectedAt selected ) handler) = selected


expand
     : Core.Control
            setup
            ( NestState, a )
            msg
    -> Core.Control
            setup
            ( NestState, a )
            msg
expand (Core.Control setup ( _, a ) handler) =
    Core.Control setup ( Expanded, a ) handler


collapse
     : Core.Control
            setup
            ( NestState, a )
            msg
    -> Core.Control
            setup
            ( NestState, a )
            msg
collapse (Core.Control setup ( _, a ) handler) =
    Core.Control setup ( Collapsed, a ) handler


detach
     : Core.Control
            setup
            ( NestState, a )
            msg
    -> Core.Control
            setup
            ( NestState, a )
            msg
detach (Core.Control setup ( _, a ) handler) =
    Core.Control setup ( Detached, a ) handler


attach
     : Core.Control
            setup
            ( NestState, a )
            msg
    -> Core.Control
            setup
            ( NestState, a )
            msg
attach = expand


execute
    : Core.Control
            setup
            ( NestState, a )
            msg
    -> Core.Control
            setup
            ( NestState, a )
            msg
execute (Core.Control setup ( expanded, a ) handler) =
    let
        nextState =
            case expanded of
                Collapsed -> Expanded
                Expanded -> Collapsed
                Detached -> Detached
    in
        Core.Control setup ( nextState, a ) handler



getState : Core.Control a ( NestState, b ) msg -> NestState
getState (Core.Control _ ( state, _ ) handler) = state


is : NestState -> Core.Control setup ( NestState, a ) msg -> Bool
is state (Core.Control _ ( expanded, a ) _) = expanded == state


getItems
     : Core.Control
            ( a, Array item )
            value
            msg
    -> Array item
getItems (Core.Control ( _, items ) _ _) = items


setItems
     : Array item
    -> Core.Control ( a, Array item ) value msg
    -> Core.Control ( a, Array item ) value msg
setItems newItems (Core.Control ( a, items ) value handler) =
    Core.Control ( a, newItems ) value handler


mapItems
     : (itemA -> itemB)
    -> Core.Control
            ( a, Array itemA )
            value
            msg
    -> Core.Control
            ( a, Array itemB )
            value
            msg
mapItems f (Core.Control ( a, items ) value handler) =
    Core.Control ( a, items |> Array.map f ) value handler


indexedMapItems
     : (Int -> itemA -> itemB)
    -> Core.Control
            ( a, Array itemA )
            value
            msg
    -> Core.Control
            ( a, Array itemB )
            value
            msg
indexedMapItems f (Core.Control ( a, items ) value handler) =
    Core.Control ( a, items |> Array.indexedMap f ) value handler
