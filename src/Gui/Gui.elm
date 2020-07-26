module Gui.Gui exposing
    ( Model
    , view, update, build, none, map
    -- , moves, ups, downs
    , trackMouse
    , fromAlt -- FIXME: temporary
    )


import Browser.Events as Browser


import Gui.Def exposing (..)
import Gui.Msg exposing (..)
import Gui.Nest exposing (..)
import Gui.Render.Grid as Render exposing (..)
import Gui.Mouse exposing (..)
import Gui.Util exposing (..)
import Gui.Alt as Alt exposing (Gui)


type alias Model umsg = ( MouseState, Nest umsg )
-- type alias View umsg = Render.Grid umsg
-- type alias Msg = Gui.Msg.Msg


view : Model umsg -> Render.GridView umsg
view = Tuple.second >> Render.view


--moves mstate = Gui.Mouse.moves mstate >> TrackMouse
moves gui pos = extractMouse gui |> Gui.Mouse.moves pos
--ups mstate = Gui.Mouse.ups mstate >> TrackMouse
ups gui pos = extractMouse gui |> Gui.Mouse.ups pos
--downs mstate = Gui.Mouse.downs mstate >> TrackMouse
downs gui pos = extractMouse gui |> Gui.Mouse.downs pos


extractMouse : Model umsg -> MouseState
extractMouse = Tuple.first


build : Nest umsg -> Model umsg
build nest =
    ( Gui.Mouse.init, nest )


none : Model umsg
none =
    ( Gui.Mouse.init, noChildren )


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
                                        TurnedOff -> toMsg Alt.On
                                        TurnedOn ->toMsg Alt.Off
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
    -> Model umsg
update msg ( ( mouse, ui ) as model ) =
    withMouse mouse <| case msg of

        FocusOn pos ->
            ui |> shiftFocusTo pos

        Tune pos alter ->
            ui
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
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ handler ->
                                Toggle label TurnedOn handler
                            _ -> cell
                    )

        ToggleOff pos ->
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Toggle label _ handler ->
                                Toggle label TurnedOff handler
                            _ -> cell
                    )

        ExpandNested pos ->
            ui
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
            ui
                |> shiftFocusTo pos
                |> updateCell pos
                    (\cell ->
                        case cell of
                            Nested label _ cells ->
                                Nested label Collapsed cells
                            _ -> cell
                    )

        ExpandChoice pos ->
            ui
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
            ui
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
                ui
                    |> shiftFocusTo pos
                    |> updateCell parentPos
                        (\cell ->
                            case cell of
                                Choice label expanded selection handler cells ->
                                    Choice label expanded index handler cells
                                _ -> cell
                        )

        ShiftFocusLeftAt pos ->
            ui |> shiftFocusBy -1 pos

        ShiftFocusRightAt pos ->
            ui |> shiftFocusBy 1 pos

        NoOp ->
            ui


withMouse : MouseState -> Nest umsg -> Model umsg
withMouse = Tuple.pair


trackMouse : Model umsg -> Sub ( Msg, Maybe umsg )
trackMouse gui =
    Sub.batch
        [ Sub.map (downs gui)
            <| Browser.onMouseDown
            <| decodePosition
        , Sub.map (ups gui)
            <| Browser.onMouseUp
            <| decodePosition
        , Sub.map (moves gui)
            <| Browser.onMouseMove
            <| decodePosition
        ]
    |> Sub.map (trackMouse_ gui)


trackMouse_ : Model umsg -> MouseState -> ( Msg, Maybe umsg )
trackMouse_ ( prevMouseState, ui ) nextMouseState =
    let (Focus focusedPos) = findFocus ui
    in
        case findCell focusedPos ui of
            Just (Knob _ knobState curValue handler) ->
                let alter = applyMove prevMouseState nextMouseState knobState curValue
                in
                    ( Tune focusedPos alter
                    ,
                        if (prevMouseState.down == True && nextMouseState.down == False)
                        then Just <| handler (alterKnob knobState alter curValue)
                        else Nothing
                    )
            _ -> ( NoOp, Nothing )


map : (msgA -> msgB) -> Model msgA -> Model msgB
map f ( mouse, nest ) =
    ( mouse
    , mapNest f nest
    )


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
