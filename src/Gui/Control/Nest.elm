module Gui.Control.Nest exposing (..)


import Array exposing (Array)

import Gui.Control as Core exposing (Control)

import Gui.Style.CellShape exposing (CellShape)


type alias Shape = ( Float, Float )


type NestState
    = Expanded
    | Collapsed
    | Detached


type FocusAt = FocusAt Int


type SelectedAt = SelectedAt Int


-- TODO: move focus outside, only selection matters in the component for logic
-- TODO: may be even (Shape, CellShape) should also be outside

type alias GroupControl item msg =
    Core.Control
        ( ( Shape, CellShape ), Array item )
        ( NestState, Maybe FocusAt )
        msg


type alias ChoiceControl item msg =
    Core.Control
        ( ( Shape, CellShape ), Array item )
        ( NestState, ( Maybe FocusAt, SelectedAt ) )
        msg


select : Int -> ChoiceControl item msg -> ChoiceControl item msg
select index (Core.Control setup ( expanded, ( focus, _ ) ) handler) =
    Core.Control setup ( expanded, ( focus, SelectedAt index ) ) handler


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
