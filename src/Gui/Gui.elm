module Gui.Gui exposing
    ( Gui
    , view, update, none, map
    , trackMouse, focus, fromWindow
    )


import Browser.Dom as Dom
import Task
import Browser.Events as Browser
import Html exposing (Html)

import BinPack exposing (..)

import Gui.Control exposing (..)
import Gui.Over exposing (..)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (..)
import Gui.Msg exposing (..)
import Gui.Render.Layout as Layout exposing (..)
import Gui.Mouse exposing (..)
import Gui.Mouse as Mouse exposing (..)
import Gui.Util exposing (..)
-- import Gui.Alt as Alt exposing (Gui)
import Gui.Focus as Focus exposing (..)


type alias Gui msg =
    { mouse : MouseState
    , tree : Over msg
    , layout : Layout
    }


moves size gui =
    Gui.Mouse.subPos (offsetFromSize size gui)
        >> Gui.Mouse.Move
ups size gui =
    Gui.Mouse.subPos (offsetFromSize size gui)
        >> Gui.Mouse.Up
downs size gui =
    Gui.Mouse.subPos (offsetFromSize size gui)
        >> Gui.Mouse.Down


extractMouse : Gui msg -> MouseState
extractMouse = .mouse


map : (msgA -> msgB) -> Gui msgA -> Gui msgB
map f model =
    { mouse = model.mouse
    , tree = Gui.Over.map f model.tree
    , layout = model.layout
    }


none : Gui msg
none =
    Gui Gui.Mouse.init Nil
        <| BinPack.container Layout.maxCellsByX Layout.maxCellsByY


update
    :  Msg
    -> Gui umsg
    -> ( Gui umsg, Cmd umsg )
update msg gui =
    case msg of
        NoOp ->
            ( gui, Cmd.none )

        ApplyMouse mouseAction ->
            handleMouse mouseAction gui

        Click path ->
            let
                (nextRoot, cmds) =
                    gui.tree |> executeAt path
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , cmds
                )

        MouseDown path ->
            (
                { gui
                | tree = gui.tree |> Focus.on path
                }
            , Cmd.none
            )

        KeyDown keyCode ->
           let
                curFocus = Focus.find gui.tree
                (nextRoot, cmds) =
                    gui.tree |> handleKeyDown keyCode curFocus
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , cmds
                )


trackMouse :  { width : Int, height : Int } -> Layout -> Sub Msg
trackMouse windowSize gui =
    Sub.batch
        [ Sub.map (downs windowSize gui)
            <| Browser.onMouseDown
            <| decodePosition
        , Sub.map (ups windowSize gui)
            <| Browser.onMouseUp
            <| decodePosition
        , Sub.map (moves windowSize gui)
            <| Browser.onMouseMove
            <| decodePosition
        ]
    |> Sub.map ApplyMouse


handleMouse : MouseAction -> Gui msg -> ( Gui msg, Cmd msg )
handleMouse mouseAction gui =
    let
        curMouseState = gui.mouse
        nextMouseState = curMouseState |> Gui.Mouse.apply mouseAction -- FIXME: calculate once
        maybeDragFromPos =
            if nextMouseState.down then nextMouseState.dragFrom
            else case mouseAction of
                Mouse.Up _ ->
                    if curMouseState.down
                        then curMouseState.dragFrom
                        else Nothing
                _ -> Nothing
        dragFromCell =
            maybeDragFromPos
                |> Maybe.andThen
                    (\dragFromPos ->

                        gui.layout
                            |> BinPack.find dragFromPos
                            -- |> Maybe.andThen Gui.Over.find
                            |> Maybe.andThen
                                (\path ->
                                    Gui.Over.find path gui.tree
                                        |> Maybe.map (Tuple.pair path)
                                )

                    )
    in

        {- FIMXE:
        dragFromPath
            = maybeDragFromPos |> Maybe.andThen (model.layout >> BinPack.find dragFromPos)

        case dragFromPath of
            Just path ->
                { model
                | mouse = nextMouseState
                , root =
                    updateAt
                        path
                        (\curControl -> case curControl of  )
                }
        -}

        case dragFromCell of

            Just ( path, Number ( Control axis curValue handler ) ) ->
                let
                    dY = distanceY knobDistance nextMouseState
                    nextVal = alter axis dY curValue
                    nextControl =
                        Control axis nextVal handler
                in
                    (
                        { gui
                        | mouse = nextMouseState
                        , tree =
                            updateAt
                                path
                                (always <| Number nextControl)
                                gui.tree
                        }
                    ,
                        case mouseAction of
                            Mouse.Up _ ->
                                if curMouseState.down
                                && not nextMouseState.down
                                then call nextControl
                                else Cmd.none
                            _ -> Cmd.none
                    )

            Just ( path, Coordinate ( Control ( xAxis, yAxis ) ( curX, curY ) handler ) ) ->
                let
                    ( dX, dY ) = distanceXY knobDistance nextMouseState
                    ( nextX, nextY ) =
                        ( alter xAxis dX curX
                        , alter yAxis dY curY
                        )
                    nextControl =
                        Control ( xAxis, yAxis ) ( nextX, nextY ) handler
                in
                    (
                        { gui
                        | mouse = nextMouseState
                        , tree =
                            updateAt
                                path
                                (always <| Coordinate nextControl)
                                gui.tree
                        }
                    ,
                        case mouseAction of
                            Mouse.Up _ ->
                                if curMouseState.down
                                && not nextMouseState.down
                                then call nextControl
                                else Cmd.none
                            _ -> Cmd.none
                    )

            _ ->
                (
                    { gui
                    | mouse = nextMouseState
                    }
                , Cmd.none
                )


handleKeyDown
    :  Int
    -> Path
    -> Over msg
    -> ( Over msg, Cmd msg )
handleKeyDown keyCode path root =
    case keyCode of
        -- left arrow
        37 -> ( root |> Focus.shift Focus.Left, Cmd.none )
        -- right arrow
        39 -> ( root |> Focus.shift Focus.Right, Cmd.none )
        -- up arrow
        38 -> ( root |> Focus.shift Focus.Up, Cmd.none )
        -- down arrow
        40 -> ( root |> Focus.shift Focus.Down, Cmd.none )
        -- space
        33 -> root |> executeAt path
        -- enter
        13 -> root |> executeAt path
        -- else
        _ -> ( root, Cmd.none )



updateWith : ( Msg, Maybe msg ) -> Gui msg -> ( Gui msg, Cmd msg  )
updateWith ( msg, maybeUserMsg ) model =
    update msg model
        |> Tuple.mapSecond
            (\cmd ->
                Cmd.batch
                    [ cmd
                    , maybeUserMsg
                        |> Maybe.map Task.succeed
                        |> Maybe.map (Task.perform identity)
                        |> Maybe.withDefault Cmd.none
                    ]
            )


focus : msg -> Cmd msg
focus noOp =
    Dom.focus Layout.rootId
        |> Task.attempt (always noOp)


fromWindow : (Int -> Int -> msg) -> Cmd msg
fromWindow passSize =
    Dom.getViewport
        |> Task.perform
            (\d -> passSize
                (floor d.viewport.width)
                (floor d.viewport.height)
            )


{-
-- FIXME: move somewhere else, where it belongs
executeCell : GridCell umsg -> ( Msg umsg, Maybe umsg )
executeCell { cell, nestPos, isSelected, onSelect } =
    case cell of
        Knob _ _ _ _ ->
            ( FocusOn nestPos, Nothing ) -- FIXME: NoOp? We do the same on mousedown
        Toggle _ val handler ->
            -- if val == TurnedOn then ToggleOff nestPos else ToggleOn nestPos
            if val == TurnedOn
            then ( ToggleOff nestPos, Just <| handler TurnedOff )
            else ( ToggleOn nestPos, Just <| handler TurnedOn )
        Nested _ state _ ->
            if state == Expanded
            then ( CollapseNested nestPos, Nothing )
            else ( ExpandNested nestPos, Nothing )
        Choice _ state _ _ _ ->
            if state == Expanded
            then ( CollapseChoice nestPos, Nothing )
            else ( ExpandChoice nestPos, Nothing )
        Button _ handler ->
            ( NoOp, Just <| handler () )
        ChoiceItem label ->
            case isSelected of
                Just NotSelected ->
                    ( Select nestPos
                    , onSelect
                        |> Maybe.andThen ((|>) label)
                    )
                _ -> ( NoOp, Nothing )
        _ ->
            case isSelected of
                Just NotSelected -> ( Select nestPos, Nothing )
                _ -> ( NoOp, Nothing )
-}


offsetFromSize : { width : Int, height : Int } -> Layout -> { x : Float, y : Float }
offsetFromSize { width, height } layout =
    let
        ( gridWidthInCells, gridHeightInCells ) = getSize layout
        ( gridWidthInPx, gridHeightInPx ) =
            ( cellWidth * gridWidthInCells
            , cellHeight * gridHeightInCells
            )
    in
        { x = (toFloat width / 2) - (toFloat gridWidthInPx / 2)
        , y = (toFloat height / 2) - (toFloat gridHeightInPx / 2)
        }


view : Gui msg -> Html Msg
view gui = Layout.view gui.tree gui.layout
