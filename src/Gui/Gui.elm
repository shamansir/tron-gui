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
    :  Msg
    -> Model umsg
    -> ( Model umsg, Cmd umsg )
update msg ( { root, mouse } as model ) =
    case msg of
        ApplyMouse mouseAction ->
            let
                nextMouseState = mouse |> Gui.Mouse.apply mouseAction -- FIXME: calculate once
                (Focus focusedPos) = findFocus root
                -- prevCell = Debug.log "prev" <| findCellAt prevMouseState.pos <| layout ui
                -- _ = Debug.log "pos" nextMouseState.pos
                nextCell = findCellAt nextMouseState.pos <| layout root  -- FIXME: store
            in
                -- case Debug.log "nextCell" nextCell of
                case nextCell of
                    -- case findCell focusedPos ui of
                    Just (Knob _ knobState curValue handler) ->
                        let
                            alter = applyMove mouse nextMouseState knobState curValue
                            -- _ = Debug.log "prevMouseState" mouse
                            -- _ = Debug.log "nextMouseState" nextMouseState
                        in
                            update (Tune focusedPos alter) model
                            |> Tuple.mapSecond
                                (\cmd ->
                                    Cmd.batch
                                        [ cmd
                                        , if (mouse.down == True && nextMouseState.down == False)
                                            then
                                                handler (alterKnob knobState alter curValue)
                                                    |> Task.succeed
                                                    |> Task.perform identity
                                            else Cmd.none
                                        ]
                                )
                    _ -> ( model, Cmd.none )
        _ -> (
            { model
            | root =
                case msg of

                    ApplyMouse _ -> root

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
