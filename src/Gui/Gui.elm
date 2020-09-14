module Gui.Gui exposing
    ( Gui, over
    , view, update, none, map
    , trackMouse, focus, reflow, fromWindow
    )


import Browser.Dom as Dom
import Task
import Browser.Events as Browser
import Html exposing (Html)

import BinPack exposing (..)
import Bounds exposing (Bounds)

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (..)
import Gui.Msg exposing (..)
import Gui.Render.Layout as Layout exposing (..)
import Gui.Mouse exposing (..)
import Gui.Mouse as Mouse exposing (..)
import Gui.Util exposing (..)
-- import Gui.Alt as Alt exposing (Gui)
import Gui.Focus as Focus exposing (..)


type Flow
    = TopToBottom
    | BottomToTop
    | LeftToRight
    | RightToLeft


type alias Gui msg =
    { flow : Flow
    , bounds : Bounds
    , mouse : MouseState
    , tree : Property msg
    , layout : Layout
    }


moves : Position -> MouseAction
moves = Gui.Mouse.Move
ups : Position -> MouseAction
ups = Gui.Mouse.Up
downs : Position -> MouseAction
downs = Gui.Mouse.Down


extractMouse : Gui msg -> MouseState
extractMouse = .mouse


map : (msgA -> msgB) -> Gui msgA -> Gui msgB
map f model =
    { flow = model.flow
    , bounds = model.bounds
    , mouse = model.mouse
    , tree = Gui.Property.map f model.tree
    , layout = model.layout
    }


none : Gui msg
none =
    Gui TopToBottom Bounds.zero Gui.Mouse.init Nil Layout.init


over : Property msg -> Gui msg
over prop =
    Gui TopToBottom Bounds.zero Gui.Mouse.init prop <| Layout.pack prop


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
                    , layout = Layout.pack nextRoot
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
                    , layout = Layout.pack nextRoot
                    }
                , cmds
                )


trackMouse : Sub Msg
trackMouse =
    Sub.batch
        [ Sub.map downs
            <| Browser.onMouseDown
            <| decodePosition
        , Sub.map ups
            <| Browser.onMouseUp
            <| decodePosition
        , Sub.map moves
            <| Browser.onMouseMove
            <| decodePosition
        ]
    |> Sub.map ApplyMouse



reflow : ( Int, Int ) -> Gui msg -> Gui msg
reflow ( w, h ) gui =
    { gui
    | bounds =
        gui.layout |>
            boundsFromSize
                { width = toFloat w, height = toFloat h }
    }


-- TODO: make bounds to be bounded to pariticular units
toGridCoords : Bounds -> Flow -> Position -> Position
toGridCoords bounds flow pos =
    { x = (pos.x - bounds.x) / cellWidth
    , y = (pos.y - bounds.y) / cellHeight
    }


handleMouse : MouseAction -> Gui msg -> ( Gui msg, Cmd msg )
handleMouse mouseAction gui =
    let
        curMouseState =
            gui.mouse
        nextMouseState =
            gui.mouse
                |> Gui.Mouse.apply mouseAction

        findCellAt pos =
            pos
                |> toGridCoords gui.bounds gui.flow
                |> (\gridPos ->

                        gui.layout
                            |> Layout.find gridPos
                            |> Maybe.andThen
                                (\path ->
                                    Gui.Property.find path gui.tree
                                        |> Maybe.map (Tuple.pair path)
                                )
                   )

        dragFromCell =
            nextMouseState.dragFrom
                |> Maybe.map (toGridCoords gui.bounds gui.flow)
                |> Maybe.andThen
                    (\dragFromPos ->

                        gui.layout
                            |> Layout.find dragFromPos
                            |> Maybe.andThen
                                (\path ->
                                    Gui.Property.find path gui.tree
                                        |> Maybe.map (Tuple.pair path)
                                )

                    )

    in

        (
            { gui
            | mouse = nextMouseState
            , tree =

                case nextMouseState.dragFrom |> Maybe.andThen findCellAt of

                    Just ( path, Number ( Control axis curValue handler ) ) ->
                        let
                            dY = distanceY knobDistance nextMouseState
                            nextVal = alter axis dY curValue
                            nextControl =
                                Control axis nextVal handler
                        in
                            updateAt
                                path
                                (always <| Number nextControl)
                                gui.tree

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
                            updateAt
                                path
                                (always <| Coordinate nextControl)
                                gui.tree

                    _ ->
                        gui.tree

            }

        ,

            case mouseAction of

                Mouse.Up _ ->
                    if curMouseState.down
                    && not nextMouseState.down
                    then

                        case curMouseState.dragFrom |> Maybe.andThen findCellAt of

                            Just ( _, Number control ) ->
                                call control
                            Just ( _, Coordinate control ) ->
                                call control
                            Just (_, _) -> Cmd.none
                            Nothing -> Cmd.none

                    else Cmd.none
                _ -> Cmd.none

        )


handleKeyDown
    :  Int
    -> Path
    -> Property msg
    -> ( Property msg, Cmd msg )
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


fromWindow : (Int -> Int -> msg) -> Cmd msg -- FIXME: get rid of
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


boundsFromSize : { width : Float, height : Float } -> Layout -> Bounds
boundsFromSize { width, height } layout =
    let
        ( gridWidthInCells, gridHeightInCells ) = getSize layout
        ( gridWidthInPx, gridHeightInPx ) =
            ( cellWidth * toFloat gridWidthInCells
            , cellHeight * toFloat gridHeightInCells
            )
    in
        { x = (width / 2) - (gridWidthInPx / 2)
        , y = (height / 2) - (gridHeightInPx / 2)
        , width = gridWidthInPx
        , height = gridHeightInPx
        }


view : Gui msg -> Html Msg
view gui =
    Layout.view gui.bounds gui.tree gui.layout
