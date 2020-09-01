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
            ( model.root |> focusOn path
            , Cmd.none
            )

        KeyDown keyCode focus_ maybeCells ->
            model
                |> updateWith
                    (handleKeyDown focus_ maybeCells keyCode)

        _ -> (
            { model
            | root =
                case msg of

                    ApplyMouse _ -> root

                    Click _ -> root

                    MouseDown _ -> root

                    KeyDown _ _ _ -> root

                    FocusOn pos ->
                        root |> Focus.on (Focus pos)

                    Tune pos alter ->
                        root
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Knob label setup curValue handler ->
                                            Knob label setup
                                                (alterKnob setup alter curValue)
                                                handler
                                        _ -> cell
                                )

                    TuneXY pos alter ->
                        root
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        XY label setup curValue handler ->
                                            XY label setup
                                                (alterXY setup alter curValue)
                                                handler
                                        _ -> cell
                                )

                    ToggleOn pos ->
                        root
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Toggle label _ handler ->
                                            Toggle label TurnedOn handler
                                        _ -> cell
                                )

                    ToggleOff pos ->
                        root
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Toggle label _ handler ->
                                            Toggle label TurnedOff handler
                                        _ -> cell
                                )

                    ExpandNested pos ->
                        root
                            |> Focus.on (Focus pos)
                            |> collapseAllAbove pos
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Nested label _ cells ->
                                            Nested label Expanded cells
                                        _ -> cell
                                )

                    CollapseNested pos ->
                        root
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Nested label _ cells ->
                                            Nested label Collapsed cells
                                        _ -> cell
                                )

                    ExpandChoice pos ->
                        root
                            |> collapseAllAbove pos
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Choice label _ selection handler cells ->
                                            Choice label Expanded selection handler cells
                                        _ -> cell
                                )

                    CollapseChoice pos ->
                        root
                            |> Focus.on (Focus pos)
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Choice label _ selection handler cells ->
                                            Choice label Collapsed selection handler cells
                                        _ -> cell
                                )

                    Select pos ->
                        let
                            parentPos = getParentPos pos |> Maybe.withDefault nowhere
                            index = getIndexOf pos |> Maybe.withDefault -1
                        in
                            root
                                |> Focus.on (Focus pos)
                                |> updateCell parentPos
                                    (\cell ->
                                        case cell of
                                            Choice label expanded selection handler cells ->
                                                Choice label expanded index handler cells
                                            _ -> cell
                                    )

                    ShiftFocus direction ->
                        root |> Focus.on (Focus.get root |> Focus.shift direction)

                    NoOp ->
                        root

            }, Cmd.none )


trackMouse :  { width : Int, height : Int } -> Model umsg -> Sub Msg
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


handleMouse : MouseAction -> Model umsg -> ( Model umsg, Cmd umsg )
handleMouse mouseAction model =
    let
        curMouseState = model.mouse
        nextMouseState = curMouseState |> Gui.Mouse.apply mouseAction -- FIXME: calculate once
        maybeDragPos =
            if nextMouseState.down then nextMouseState.dragFrom
            else case mouseAction of
                Mouse.Up _ -> if curMouseState.down then curMouseState.dragFrom else Nothing
                _ -> Nothing
        nextCell =
            maybeDragPos
                |> Maybe.andThen
                    (\dragPos ->
                        layout model.root   -- FIXME: store layout in the Model
                            |> findCellAt dragPos
                            |> Maybe.map (\c -> ( c.cell, c.nestPos ))
                    )
    in
        case nextCell of
            -- case findCell focusedPos ui of

            Just ( (Knob _ knobState curValue handler), cellPos ) ->
                let
                    alter = applyKnobMove curMouseState nextMouseState knobState curValue

                in
                    { model
                    | mouse = nextMouseState
                    } |>
                        updateWith
                            ( Tune cellPos alter
                            , case mouseAction of
                                Mouse.Up _ ->
                                    if curMouseState.down
                                    && not nextMouseState.down
                                    then
                                        Just <| handler (alterKnob knobState alter curValue)
                                    else Nothing
                                _ -> Nothing
                            )

            Just ( (XY _ xyState curValue handler), cellPos ) ->
                let
                    alter = applyXYMove curMouseState nextMouseState xyState curValue

                in
                    { model
                    | mouse = nextMouseState
                    } |>
                        updateWith
                            ( TuneXY cellPos alter
                            , case mouseAction of
                                Mouse.Up _ ->
                                    if curMouseState.down
                                    && not nextMouseState.down
                                    then
                                        Just <| handler (alterXY xyState alter curValue)
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

{-
handleKeyDown
    :  Focus
    -> Maybe { current : GridCell umsg, parent : GridCell umsg }
    -> Int
    -> ( Msg, Maybe umsg )
handleKeyDown (Focus currentFocus) maybeCells keyCode =
    let
        executeCell_ =
            maybeCells
            |> Maybe.map .current
            |> Maybe.map executeCell
            |> Maybe.withDefault ( NoOp, Nothing )
    -- Find top focus, with it either doCellPurpose or ShiftFocusRight/ShiftFocusLeft
    in
        case keyCode of
            -- left arrow
            37 -> ( ShiftFocus Focus.Left, Nothing )
            -- right arrow
            -- up arrow
            38 -> ( ShiftFocus Focus.Up, Nothing )
            -- down arrow
            40 -> ( ShiftFocus Focus.Down, Nothing )
            -- space
            33 -> executeCell_
            -- enter
            13 -> executeCell_
            -- else
            _ -> ( NoOp, Nothing )
-}



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
