module Gui.Control.Nest exposing (..)


import Array exposing (Array)

import Gui.Control as Core exposing (Control)


type alias Id = Int


type NestState
    = Expanded
    | Collapsed
    | Detached


type SelectedAt = SelectedAt Int


-- TODO: move focus outside, only selection matters in the component for logic
-- TODO: may be even (Shape, CellShape) should also be outside

type alias GroupControl item msg =
    Core.Control
        ( Array item )
        ( NestState, () )
        msg


type alias ChoiceControl item msg =
    Core.Control
        ( Array item )
        ( NestState, SelectedAt )
        msg


get : Int -> Core.Control ( Array item ) value msg -> Maybe item
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
whichSelected (Core.Control _ ( _, SelectedAt selected ) _) = selected


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


getItems : Core.Control ( Array item ) value msg -> Array item
getItems (Core.Control items _ _) = items


setItems
     : Array item
    -> Core.Control ( Array item ) value msg
    -> Core.Control ( Array item ) value msg
setItems newItems (Core.Control _ value handler) =
    Core.Control newItems value handler


mapItems
     : (itemA -> itemB)
    -> Core.Control
            ( Array itemA )
            value
            msg
    -> Core.Control
            ( Array itemB )
            value
            msg
mapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.map f ) value handler


indexedMapItems
     : (Int -> itemA -> itemB)
    -> Core.Control
            ( Array itemA )
            value
            msg
    -> Core.Control
            ( Array itemB )
            value
            msg
indexedMapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.indexedMap f ) value handler


withItem
     : Int
    -> (item -> item)
    -> Core.Control (Array item) value msg
    -> Core.Control (Array item) value msg
withItem id f ( Core.Control items state handler ) =
    Core.Control
        ( case Array.get id items of
            Just item ->
                items
                |> Array.set id (f item)
            Nothing -> items
        )
        state
        handler
