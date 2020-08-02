module Gui.Gui exposing
    ( Model
    , view, update, build, none, map
    -- , moves, ups, downs
    , trackMouse
    , fromAlt -- FIXME: temporary
    )


import Task
import Browser.Events as Browser


import Gui.Def exposing (..)
import Gui.Msg exposing (..)
import Gui.Nest exposing (..)
import Gui.Grid as Grid exposing (..)
import Gui.Render.Grid as Render exposing (..)
import Gui.Mouse exposing (..)
import Gui.Util exposing (..)
import Gui.Alt as Alt exposing (Gui)


type alias Model umsg =
    { mouse : MouseState
    , root : Nest umsg
    }
-- type alias View umsg = Render.Grid umsg
-- type alias Msg = Gui.Msg.Msg


view : Model umsg -> Render.GridView umsg
view = .root >> Render.view


moves size gui =
    Gui.Mouse.subPos (offsetFromSize size gui)
        >> Gui.Mouse.Move
ups size gui =
    Gui.Mouse.subPos (offsetFromSize size gui)
        >> Gui.Mouse.Up
downs size gui =
    Gui.Mouse.subPos (offsetFromSize size gui)
        >> Gui.Mouse.Down


extractMouse : Model umsg -> MouseState
extractMouse = .mouse


build : Nest umsg -> Model umsg
build =
    Model Gui.Mouse.init


none : Model umsg
none =
    Model Gui.Mouse.init noChildren


fromAlt : Alt.Gui umsg -> Nest umsg
fromAlt altGui =
    let
        cells =
            altGui
                |> List.map (\(_, label, prop) ->
                    case prop of
                        Alt.Ghost ->
                            Ghost label
                        Alt.Slider { min, max, step } current toMsg ->
                            Knob
                                label
                                { min = min, max = max, step = step, roundBy = 2, default = current }
                                current
                                toMsg
                        Alt.Input curent toMsg ->
                            Ghost label -- TODO
                        Alt.Color curent toMsg ->
                            Ghost label -- TODO
                        Alt.Toggle current toMsg ->
                            Toggle
                                label
                                (case current of
                                    Alt.On -> TurnedOn
                                    Alt.Off -> TurnedOff
                                )
                                (\next ->
                                    case next of
                                        TurnedOn ->toMsg Alt.On
                                        TurnedOff -> toMsg Alt.Off
                                )
                        Alt.Button toMsg ->
                            Button label toMsg
                        Alt.Choice options maybeCurrent toMsg ->
                            Choice label Expanded (maybeCurrent |> Maybe.withDefault 0)
                                (\idx _ -> toMsg idx)
                                <| oneLine
                                    (options
                                        |> List.map Tuple.second
                                        |> List.map ChoiceItem)
                        Alt.Nested expanded gui ->
                            Nested
                                label
                                (case expanded of
                                    Alt.Expanded -> Expanded
                                    Alt.Collapsed -> Collapsed
                                )
                                <| fromAlt gui
                )
    in
        oneLine cells


update
    :  Msg umsg
    -> Model umsg
    -> ( Model umsg, Cmd umsg )
update msg ( { root, mouse } as model ) =
    case msg of

        ApplyMouse mouseAction ->
            handleMouse mouseAction model

        Click cell ->
            let
                ( nextMsg, maybeUserMsg  )
                    = executeCell cell
            in
                update nextMsg model
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

        MouseDown { cell, nestPos } ->
            case cell of
                Knob _ _ _ _ ->
                    update (FocusOn nestPos) model
                _ -> ( model, Cmd.none )

        _ -> (
            { model
            | root =
                case msg of

                    ApplyMouse _ -> root

                    Click _ -> root

                    MouseDown _ -> root

                    KeyDown _ _ -> root

                    FocusOn pos ->
                        root |> shiftFocusTo pos

                    Tune pos alter ->
                        root
                            |> shiftFocusTo pos
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Knob label setup curValue handler ->
                                            Knob label setup
                                                (alterKnob setup alter curValue)
                                                handler
                                        _ -> cell
                                )

                    ToggleOn pos ->
                        root
                            |> shiftFocusTo pos
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Toggle label _ handler ->
                                            Toggle label TurnedOn handler
                                        _ -> cell
                                )

                    ToggleOff pos ->
                        root
                            |> shiftFocusTo pos
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Toggle label _ handler ->
                                            Toggle label TurnedOff handler
                                        _ -> cell
                                )

                    ExpandNested pos ->
                        root
                            |> shiftFocusTo pos
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
                            |> shiftFocusTo pos
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
                            |> shiftFocusTo pos
                            |> updateCell pos
                                (\cell ->
                                    case cell of
                                        Choice label _ selection handler cells ->
                                            Choice label Expanded selection handler cells
                                        _ -> cell
                                )

                    CollapseChoice pos ->
                        root
                            |> shiftFocusTo pos
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
                                |> shiftFocusTo pos
                                |> updateCell parentPos
                                    (\cell ->
                                        case cell of
                                            Choice label expanded selection handler cells ->
                                                Choice label expanded index handler cells
                                            _ -> cell
                                    )

                    ShiftFocusLeftAt pos ->
                        root |> shiftFocusBy -1 pos

                    ShiftFocusRightAt pos ->
                        root |> shiftFocusBy 1 pos

                    NoOp ->
                        root

            }, Cmd.none )


trackMouse :  { width : Int, height : Int } -> Model umsg -> Sub (Msg umsg)
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
        nextMouseState = model.mouse |> Gui.Mouse.apply mouseAction -- FIXME: calculate once
        (Focus focusedPos) = findFocus model.root
        -- prevCell = Debug.log "prev" <| findCellAt prevMouseState.pos <| layout ui
        -- _ = Debug.log "pos" nextMouseState.pos
        nextCell = findCellAt nextMouseState.pos <| layout model.root  -- FIXME: store
    in
        -- case Debug.log "nextCell" nextCell of
        case nextCell of
            -- case findCell focusedPos ui of
            Just (Knob _ knobState curValue handler) ->
                let
                    alter = applyMove model.mouse nextMouseState knobState curValue
                    -- _ = Debug.log "prevMouseState" mouse
                    -- _ = Debug.log "nextMouseState" nextMouseState
                in
                    update (Tune focusedPos alter) model
                    |> Tuple.mapSecond
                        (\cmd ->
                            Cmd.batch
                                [ cmd
                                , if (model.mouse.down == True && nextMouseState.down == False)
                                    then
                                        handler (alterKnob knobState alter curValue)
                                            |> Task.succeed
                                            |> Task.perform identity
                                    else Cmd.none
                                ]
                        )
            _ -> ( model, Cmd.none )



keyDownHandler : Nest umsg -> Grid umsg -> Int -> ( Msg umsg, Maybe umsg )
keyDownHandler nest grid keyCode =
    let
        (Focus currentFocus) = findFocus nest
        maybeCurrentCell = findGridCell currentFocus grid
        executeCell_ = maybeCurrentCell
            |> Maybe.map executeCell
            |> Maybe.withDefault ( NoOp, Nothing )
    -- Find top focus, with it either doCellPurpose or ShiftFocusRight/ShiftFocusLeft
    in
        case keyCode of
            -- left arrow
            37 -> ( ShiftFocusLeftAt currentFocus, Nothing )
            -- right arrow
            39 -> ( ShiftFocusRightAt currentFocus, Nothing )
            -- up arrow
            -- 38 -> ShiftFocusUpAt currentFocus
            -- down arrow
            -- 40 -> ShiftFocusDownAt currentFocus
            -- up arrow
            38 -> maybeCurrentCell
                |> Maybe.map (\{ cell } ->
                        ( case cell of
                            Nested _ Collapsed _ -> ExpandNested currentFocus
                            Choice _ Collapsed _ _ _ -> ExpandChoice currentFocus
                            _ -> NoOp
                        , Nothing
                        )
                    )
                |> Maybe.withDefault ( NoOp, Nothing ) -- execute as well?
            -- down arrow
            40 -> let parentFocus = currentFocus |> shallower in
                ( if (isSamePos parentFocus nowhere)
                    then NoOp
                    else
                        findGridCell parentFocus grid
                            |> Maybe.map (\{ cell } ->
                                    case cell of
                                        Nested _ Expanded _ -> CollapseNested parentFocus
                                        Choice _ Expanded _ _ _ -> CollapseChoice parentFocus
                                        _ -> NoOp
                                )
                            |> Maybe.withDefault NoOp
                , Nothing
                )
            -- space
            33 -> executeCell_
            -- enter
            13 -> executeCell_
            -- else
            _ -> ( NoOp, Nothing )




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



{-
trackMouse_ : Model umsg -> MouseState -> ( Msg, Maybe umsg )
trackMouse_ ( prevMouseState, ui ) nextMouseState =
    let
        (Focus focusedPos) = findFocus ui
        -- prevCell = Debug.log "prev" <| findCellAt prevMouseState.pos <| layout ui
        -- _ = Debug.log "pos" nextMouseState.pos
        nextCell = findCellAt nextMouseState.pos <| layout ui  -- FIXME: store layout in the model
    in
        case Debug.log "nextCell" nextCell of
        -- case findCell focusedPos ui of
            Just (Knob _ knobState curValue handler) ->
                let
                    alter = applyMove prevMouseState nextMouseState knobState curValue
                    _ = Debug.log "prevMouseState" prevMouseState
                    _ = Debug.log "nextMouseState" nextMouseState
                in
                    ( Tune focusedPos alter
                    ,
                        if (prevMouseState.down == True && nextMouseState.down == False)
                        then Just <| handler (alterKnob knobState alter curValue)
                        else Nothing
                    )
            _ -> ( NoOp, Nothing ) -}


offsetFromSize : { width : Int, height : Int } -> Model msg -> { x : Int, y : Int }
offsetFromSize { width, height } { root } =
    let
        ( gridWidthInPx, gridHeightInPx ) =
            layout root |> getSizeInPixels -- FIXME: store layout in the model
    in
        { x = floor <| (toFloat width / 2) - (toFloat gridWidthInPx / 2)
        , y = floor <| (toFloat height / 2) - (toFloat gridHeightInPx / 2)
        }



map : (msgA -> msgB) -> Model msgA -> Model msgB
map f model =
    Model model.mouse
        <| mapNest f model.root


mapNest : (msgA -> msgB) -> Nest msgA -> Nest msgB
mapNest f nest =
    { shape = nest.shape
    , focus = nest.focus
    , cells = nest.cells |> List.map (mapCell f)
    }


mapCell : (msgA -> msgB) -> Cell msgA -> Cell msgB
mapCell f cell =
    case cell of
        Ghost label -> Ghost label
        ChoiceItem label -> ChoiceItem label
        Knob label state val handler ->
            Knob label state val (f << handler)
        Toggle label state handler ->
            Toggle label state (f << handler)
        Button label handler ->
            Button label (f << handler)
        Choice label expanded item handler nest ->
            Choice
                label
                expanded
                item
                (\i l -> handler i l |> Maybe.map f)
                (mapNest f nest)
        Nested label expanded nest ->
            Nested label expanded <| mapNest f nest
