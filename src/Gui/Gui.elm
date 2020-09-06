module Gui.Gui exposing
    ( Model
    , view, update, build, none, map
    , trackMouse, focus, fromWindow
    , fromAlt -- FIXME: temporary
    )


import Browser.Dom as Dom
import Task
import Browser.Events as Browser

import BinPack exposing (..)

import Gui.Control exposing (..)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (..)
import Gui.Msg exposing (..)
import Gui.Render.Grid as Render exposing (..)
import Gui.Mouse exposing (..)
import Gui.Mouse as Mouse exposing (..)
import Gui.Util exposing (..)
import Gui.Alt as Alt exposing (Gui)
import Gui.Focus as Focus exposing (..)


type alias Model msg =
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


extractMouse : Model msg -> MouseState
extractMouse = .mouse


none : Model msg
none =
    Model Gui.Mouse.init Anything
        <| BinPack.container Layout.maxCellsByX Layout.maxCellsByY


update
    :  Msg
    -> Model umsg
    -> ( Model umsg, Cmd umsg )
update msg ( { root, mouse } as model ) =
    case msg of

        ApplyMouse mouseAction ->
            handleMouse mouseAction model

        Click path ->
            let
                (nextRoot, cmds) =
                    model.root |> executeAt path
            in
                (
                    { model
                    | root = nextRoot
                    }
                , cmds
                )

        MouseDown path ->
            ( model.root |> Focus.on path
            , Cmd.none
            )

        KeyDown keyCode ->
           let
                curFocus = Focus.find model.root
                (nextRoot, cmds) =
                    model.root |> handleKeyDown keyCode curFocus
            in
                (
                    { model
                    | root = nextRoot
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


handleMouse : MouseAction -> Model msg -> ( Model msg, Cmd msg )
handleMouse mouseAction model =
    let
        curMouseState = model.mouse
        nextMouseState = curMouseState |> Gui.Mouse.apply mouseAction -- FIXME: calculate once
        maybeDragFromPos =
            if nextMouseState.down then nextMouseState.dragFrom
            else case mouseAction of
                Mouse.Up _ -> if curMouseState.down then curMouseState.dragFrom else Nothing
                _ -> Nothing
        dragFromCell =
            maybeDragFromPos
                |> Maybe.andThen
                    (\dragFromPos ->

                        model.layout
                            |> BinPack.find dragFromPos
                            -- |> Maybe.andThen Gui.Control.find
                            |> Maybe.andThen
                                (\path ->
                                    path
                                        |> Gui.Control.find
                                        |> Maybe.map (Tuple.pair path)
                                )

                    )
    in
        case dragFromCell of
            -- Just ( path, control ) ->
            --     { model
            --     | mouse = nextMouseState
            --     , root =
            --         updateAt
            --             path
            --             (\curControl -> case curControl of  )
            --     }

            Just ( path, Number ( Control axis curValue handler ) ) ->
                let
                    dY = distanceY knobDistance nextMouseState
                    nextVal = alter axis dY
                    nextControl =
                        Number <| Control axis nextVal handler
                in
                    (
                        { model
                        | mouse = nextMouseState
                        , root =
                            updateAt
                                path
                                (always nextControl)
                                model.root
                        }
                    ,
                        case mouseAction of
                            Mouse.Up _ ->
                                if curMouseState.down
                                && not nextMouseState.down
                                then
                                    Just <| handler nextVal
                                else Nothing
                            _ -> Nothing
                    )

            Just ( path, Coordinate ( Control ( xAxis, yAxis ) ( curX, curY ) handler ) ) ->
                let
                    ( dX, dY ) = distanceXY knobDistance nextMouseState
                    ( nextX, nextY ) =
                        ( alter xAxis dX
                        , alter yAxis dY
                        )
                    nextControl =
                        Number <| Control ( xAxis, yAxis ) ( nextX, nextY ) handler
                in
                    (
                        { model
                        | mouse = nextMouseState
                        , root =
                            updateAt
                                path
                                (always nextControl)
                                model.root
                        }
                    ,
                        case mouseAction of
                            Mouse.Up _ ->
                                if curMouseState.down
                                && not nextMouseState.down
                                then
                                    Just <| handler ( nextX, nextY )
                                else Nothing
                            _ -> Nothing
                    )

            _ ->
                (
                    { model
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
        38 -> ( root |> Focus.shift Focus.Up, Nothing )
        -- down arrow
        40 -> ( root |> Focus.shift Focus.Down, Nothing )
        -- space
        33 -> root |> executeAt path
        -- enter
        13 -> root |> executeAt path
        -- else
        _ -> ( NoOp, Nothing )



updateWith : ( Msg, Maybe msg ) -> Model msg -> ( Model msg, Cmd msg  )
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
    Dom.focus Render.rootId
        |> Task.attempt (always noOp)


fromWindow : (Int -> Int -> msg) -> Cmd msg
fromWindow passSize =
    Dom.getViewport
        |> Task.perform
            (\d -> passSize
                (floor d.viewport.width)
                (floor d.viewport.height)
            )


executeAt : Path -> Over msg -> ( Msg, Maybe msg )
executeAt path root =
    ( NoOp, Cmd.none )


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


offsetFromSize : { width : Int, height : Int } -> Layout -> { x : Int, y : Int }
offsetFromSize { width, height } { layout } =
    let
        ( gridWidthInCells, gridHeightInCells ) = getSize layout
        ( gridWidthInPx, gridHeightInPx ) =
            cellWidth * gridWidthInCells + cellHeight * gridHeightInCells
    in
        { x = floor <| (toFloat width / 2) - (toFloat gridWidthInPx / 2)
        , y = floor <| (toFloat height / 2) - (toFloat gridHeightInPx / 2)
        }
