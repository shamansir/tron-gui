module Tron.Core exposing
    ( State, Msg
    , view, update, init, subscriptions, run
    , dock, reshape
    , applyRaw, tryDeduce
    )


import Browser.Dom as Dom
import Browser.Events as Browser
import Task
import Color
import Html exposing (Html)
import Html.Events as HE
import Json.Encode as E
import Json.Decode as D
import Array
import Dict

import Size exposing (..)

import Tron exposing (Tron)
import Tron as Tron
import Tron.Path exposing (Path)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Control as Control
import Tron.Control.Text as Text
import Tron.Property as Property exposing (Property(..))
import Tron.Property.Paths as Property
import Tron.Property.Events as Property
import Tron.Property.Controls as Property
import Tron.Layout exposing (Layout)
import Tron.Layout as Layout exposing (..)
import Tron.Msg exposing (..)
import Tron.Render.Layout as Layout exposing (..)
import Tron.Mouse exposing (..)
import Tron.Mouse as Mouse exposing (..)
import Tron.Util exposing (..)
import Tron.FocusLogic as Focus exposing (..)
import Tron.Focus as Focus exposing (..)
import Tron.Detach as Detach exposing (ClientId)
import Tron.Detach exposing (State(..))
import Tron.Control.Value exposing (..)
import Tron.Control.Nest as Nest
import Tron.Build exposing (..)
import Tron.Expose as Exp

import Tron.Style.Dock exposing (Dock(..))
import Tron.Style.Dock as Dock exposing (..)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Logic as Style exposing (..)
import Tron.Style.Logic as Dock exposing (boundsFromSize)
import Tron.Style.Cell as Cell exposing (..)

import Tron.Property.ExposeData as Exp
import Tron.Control.Value as Control exposing (Value)
import Tron.Control.Value as Value


type alias State =
    { dock : Dock
    , viewport : Size Pixels
    , size : Maybe (Size Cells)
    , mouse : MouseState
    , detach : ( Maybe ClientId, Detach.State )
    , hidden : Bool
    }


type alias Msg = Msg_


moves : Mouse.Position -> MouseAction
moves = Tron.Mouse.Move
ups : Mouse.Position -> MouseAction
ups = Tron.Mouse.Up
downs : Mouse.Position -> MouseAction
downs = Tron.Mouse.Down


extractMouse : State -> MouseState
extractMouse = .mouse


init : ( State, Cmd Msg )
init =
    (
        { dock = Dock.topLeft
        , viewport = Size ( 0, 0 )
        , size = Nothing
        , mouse = Tron.Mouse.init
        , detach = ( Nothing, Detached )
        , hidden = False
        }
    , run
    )


run : Cmd Msg
run =
    Cmd.batch
        [ focus NoOp
        , fromWindow ViewportChanged
        ]

-- transferTransientState gui.tree tree


-- overMap : (msgA -> msgB) -> Property msgA -> Model msgB -> Model msgB
-- overMap f prop =
--     over <| Tron.Property.map f prop


update
    :  Msg
    -> State
    -> Tron msg
    -> ( State, Property (), Cmd (Exp.Out, msg) )
update msg state tree  =
    case msg of
        NoOp ->
            ( state, tree |> Property.toUnit, Cmd.none )

        ApplyMouse mouseAction ->
            let
                ( nextState, changesTree ) =
                    handleMouse mouseAction state (tree |> Property.toUnit)
            in
                ( nextState
                , changesTree |> Property.toUnit
                , fireChangesFrom nextState ( tree, changesTree )
                )

        Click path ->
            let
                changesTree =
                    tree |> Property.setAll Stayed
                updates =
                    changesTree
                        |> Property.executeAt path
                        |> List.map (Tuple.mapSecond <| Property.set ChangeToFire)
                nextRoot =
                    changesTree
                        |> Property.updateMany updates

            in
                ( state
                , nextRoot |> Property.toUnit
                , fireChangesFrom state ( tree, changesTree )
                )

        MouseDown path ->
            ( state
            , Focus.on tree path |> Property.toUnit
            , Cmd.none
            )

        KeyDown keyCode ->
           let
                curFocus = Focus.find tree
                ( nextState, changesTree ) =
                    handleKeyDown keyCode curFocus state (tree |> Property.toUnit)
            in
                ( nextState
                , changesTree |> Property.toUnit
                , fireChangesFrom nextState ( tree, changesTree )
                )

        ViewportChanged ( w, h ) ->
            (
                { state
                | viewport = Size (w, h)
                }
            , tree |> Property.toUnit
            , Cmd.none
            )

        TextInput path val ->
            let
                nextRoot =
                    tree
                        |> Property.updateTextAt path val
            in
                ( state
                , nextRoot |> Property.toUnit
                , Cmd.none
                )

        Detach path ->
            let
                nextRoot = Property.detachAt path tree
            in
                ( state
                , nextRoot |> Property.toUnit
                , Cmd.none -- FIXME: Detach.sendTree gui.detach nextRoot
                )

        SwitchPage path pageNum ->
            let
                nextRoot = Property.switchPageAt path pageNum tree
            in
                ( state
                , nextRoot |> Property.toUnit
                , Cmd.none
                )



applyRaw
     : Exp.In
    -> Tron msg
    -> Cmd msg
applyRaw rawUpdate =
    Exp.apply (Exp.fromPort rawUpdate)
        >> Tron.perform


applyDeduced
    :  Exp.DeduceIn
    -> Tron msg
    -> Cmd msg
applyDeduced toDeduce tree =
    case tryDeduce (tree |> Property.toUnit) toDeduce of
        Just rawUpdate -> tree |> applyRaw rawUpdate
        Nothing -> Cmd.none


applyHandlers : Tron msg -> Property (Maybe msg)
applyHandlers = Property.apply


tryDeduce : Property () -> Exp.DeduceIn -> Maybe Exp.In
tryDeduce tree { path, value } =
    tree
        |> Property.find (Path.fromList path)
        |> Maybe.map
            (\prop ->
                { path = path
                , value =

                    case prop of
                        Choice _ _ control ->

                            case D.decodeValue D.string value of
                                Ok string ->
                                    control
                                        |> Nest.find string
                                        |> Maybe.map (Tuple.first >> E.int)
                                        |> Maybe.withDefault value
                                Err _ -> value

                        _ -> value
                , type_ = prop |> Property.getValue |> Value.getTypeString
                }
            )


trackResize : Sub Msg
trackResize = Browser.onResize <| \w h -> ViewportChanged ( w, h )


trackSpaceKey : Sub Msg
trackSpaceKey =
    -- we do it separately since it can occur outside of GUI, when it's not focused
    Browser.onKeyDown
        (D.map KeyDown HE.keyCode)


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


knobDistance = 90 -- FIXME: move to `Control.Knob`



type ValueState
    = Stayed
    | SilentChange
    | ChangeToFire


handleMouse : MouseAction -> State -> Property () -> ( State, Property ValueState )
handleMouse mouseAction state tree =
    let
        rootPath = getRootPath state
        changesTree =
            tree |> Property.setAll Stayed
        curMouseState =
            state.mouse
        nextMouseState =
            state.mouse
                |> Tron.Mouse.apply mouseAction
        size = getSizeInCells state tree
        bounds =
            Dock.boundsFromSize state.dock state.viewport size
        theLayout =
            layout state tree |> Tuple.second

        findPathAt pos =
            pos
                |> Style.toGridCoords bounds
                |> Layout.find theLayout

        findCellAt pos =
            pos
                |> findPathAt
                |> Maybe.andThen
                    (\path ->
                        Property.find path changesTree
                            |> Maybe.map (Tuple.pair path)
                    )

        keepsDragging = curMouseState.down

        startedDragging =
            case mouseAction of
                Mouse.Down _ ->
                    not curMouseState.down
                    && nextMouseState.down
                _ -> False

        finishedDragging =
            case mouseAction of
                Mouse.Up _ ->
                    curMouseState.down
                    && not nextMouseState.down
                _ -> False

        treeAfterCheckIfDraggingInProcess : Property ValueState
        treeAfterCheckIfDraggingInProcess =
            if keepsDragging then

                case nextMouseState.dragFrom |> Maybe.andThen findCellAt of

                    Just ( path, Number ( Control axis ( maybeFrom, curValue ) _ ) ) ->
                        let
                            valueToAlter = maybeFrom |> Maybe.withDefault curValue
                            dY = distanceY knobDistance nextMouseState
                            nextVal = alter axis dY valueToAlter
                            nextControl =
                                Control
                                    axis
                                    ( if finishedDragging
                                        then Nothing
                                        else maybeFrom
                                    , nextVal
                                    )
                                    SilentChange
                        in
                            Property.updateAt
                                path
                                (always <| Number nextControl)
                                changesTree

                    Just ( path, Coordinate ( Control ( xAxis, yAxis ) ( maybeFrom, ( curX, curY ) ) _ ) ) ->
                        let
                            ( xToAlter, yToAlter ) = maybeFrom |> Maybe.withDefault ( curX, curY )
                            ( dX, dY ) = distanceXY knobDistance nextMouseState
                            ( nextX, nextY ) =
                                ( alter xAxis dX xToAlter
                                , alter yAxis dY yToAlter
                                )
                            nextControl =
                                Control
                                    ( xAxis, yAxis )
                                    ( if finishedDragging
                                        then Nothing
                                        else maybeFrom
                                    , ( nextX, nextY )
                                    )
                                    SilentChange
                        in
                            Property.updateAt
                                path
                                (always <| Coordinate nextControl)
                                changesTree

                    Just ( path, Color ( Control state_ ( maybeFromColor, curColor ) a ) ) ->
                        let
                            hueAxis = { min = 0, max = 1, step = 0.01 }
                            lgtAxis = { min = 0, max = 1, step = 0.01 }
                            curHsla = Color.toHsla curColor
                            ( hueToAlter, lightnessToAlter ) =
                                case maybeFromColor of
                                    Just fromColor ->
                                        case Color.toHsla fromColor of
                                            hsla -> ( hsla.hue, hsla.lightness )
                                    Nothing -> ( curHsla.hue, curHsla.lightness )
                            ( dX, dY ) = distanceXY knobDistance nextMouseState
                            ( nextHue, nextLightness ) =
                                ( alter hueAxis dX hueToAlter
                                , alter lgtAxis dY lightnessToAlter
                                )
                            nextColor =
                                Color.hsla
                                    nextHue
                                    (if curHsla.saturation > 0.25 then
                                        -- && curHsla.saturation < 0.75 then
                                            curHsla.saturation
                                    else 0.5)
                                    nextLightness -- curHsla.lightness
                                    curHsla.alpha
                            nextControl =
                                Control
                                    state_
                                    ( if finishedDragging
                                        then Nothing
                                        else maybeFromColor
                                    , nextColor
                                    )
                                    SilentChange
                        in
                            Property.updateAt
                                path
                                (always <| Color nextControl)
                                changesTree

                    Just ( path, Choice focus_ shape_ ( Control items value _ ) ) ->
                        case value.mode of
                            Nest.Knob ->
                                let
                                    valueToAlter = value.prevSelected |> Maybe.withDefault value.selected
                                    dY = distanceY knobDistance nextMouseState
                                    nextVal =
                                        alter
                                            { min = 0, max = toFloat <| Array.length items - 1, step = 1 }
                                            dY
                                            (toFloat valueToAlter)
                                    nextControl =
                                        Control
                                            items
                                            { value
                                            | selected = floor nextVal
                                            , prevSelected =
                                                if finishedDragging
                                                then Nothing
                                                else value.prevSelected
                                            }
                                            SilentChange
                                in
                                    Property.updateAt
                                        path
                                        (always <| Choice focus_ shape_ nextControl)
                                        changesTree
                            _ -> changesTree

                    _ ->
                        changesTree

            else

                let
                    refocusedTree
                        = nextMouseState.dragFrom
                            |> Maybe.andThen findPathAt
                            |> Maybe.map (Focus.on changesTree)
                            |> Maybe.withDefault changesTree
                in
                    if startedDragging then

                        case nextMouseState.dragFrom |> Maybe.andThen findCellAt of

                            Just ( path, Number ( Control axis ( _, curValue ) _ ) ) ->
                                let
                                    nextControl =
                                        Control
                                            axis
                                            ( Just curValue
                                            , curValue
                                            )
                                            SilentChange
                                in
                                    Property.updateAt
                                        path
                                        (always <| Number nextControl)
                                        refocusedTree

                            Just ( path, Coordinate ( Control axes ( _, curValue ) _ ) ) ->
                                let
                                    nextControl =
                                        Control
                                            axes
                                            ( Just curValue
                                            , curValue
                                            )
                                            SilentChange
                                in
                                    Property.updateAt
                                        path
                                        (always <| Coordinate nextControl)
                                        refocusedTree

                            Just ( path, Color ( Control state_ ( _, curColor ) _ ) ) ->
                                let
                                    nextControl =
                                        Control
                                            state_
                                            ( Just curColor
                                            , curColor
                                            )
                                            SilentChange
                                in
                                    Property.updateAt
                                        path
                                        (always <| Color nextControl)
                                        refocusedTree

                            Just ( path, Choice focus_ shape_ ( Control state_ value _ ) ) ->
                                case value.mode of
                                    Nest.Knob ->
                                        let
                                            nextControl =
                                                Control
                                                    state_
                                                    { value | prevSelected = Just value.selected }
                                                    SilentChange
                                        in
                                            Property.updateAt
                                                path
                                                (always <| Choice focus_ shape_ nextControl)
                                                refocusedTree
                                    _ -> refocusedTree

                            _ -> refocusedTree


                    else refocusedTree

        treeAfterCheckIfDraggingFinished : Property ValueState
        treeAfterCheckIfDraggingFinished =
            if finishedDragging
            then

                case curMouseState.dragFrom |> Maybe.andThen findCellAt of
                -- TODO: do we need a path returned from `findCellAt`?

                    Just ( path, prop ) ->
                        case prop of
                            Number _ ->
                                treeAfterCheckIfDraggingInProcess
                                    |> Property.setAt path ChangeToFire
                            Coordinate _ ->
                                treeAfterCheckIfDraggingInProcess
                                    |> Property.setAt path ChangeToFire
                            Color _ ->
                                treeAfterCheckIfDraggingInProcess
                                    |> Property.setAt path ChangeToFire
                            Choice _ _ (Control _ { mode } _) ->
                                case mode of
                                    Nest.Knob ->
                                        treeAfterCheckIfDraggingInProcess
                                            |> Property.setAt path ChangeToFire
                                    _ -> treeAfterCheckIfDraggingInProcess
                            _ -> treeAfterCheckIfDraggingInProcess
                    Nothing -> treeAfterCheckIfDraggingInProcess

            else treeAfterCheckIfDraggingInProcess

    in

        (
            { state
            | mouse = nextMouseState
            }
        , treeAfterCheckIfDraggingFinished
        )


handleKeyDown
    :  Int
    -> Path
    -> State
    -> Property ()
    -> ( State, Property ValueState )
handleKeyDown keyCode path state tree =
    let

        changesTree =
            tree |> Property.setAll Stayed

        executeByPath _ = -- uses only `gui.tree`
            let
                updates = changesTree |> Property.executeAt path
                markedUpdates =
                    updates |> List.map (Tuple.mapSecond <| Property.set ChangeToFire)
                nextRoot = changesTree |> Property.updateMany markedUpdates
            in
                ( state
                , nextRoot
                {- , markedUpdates
                    |> List.map (Tuple.second >> Tron.perform)
                    |> Cmd.batch
                    --|> Cmd.map (Tuple.pair path)
                -}
                )

    in case keyCode of
        -- left arrow
        37 -> ( state, changesTree |> Focus.shift Focus.Left )
        -- right arrow
        39 -> ( state, changesTree |> Focus.shift Focus.Right )
        -- up arrow
        38 -> ( state, changesTree |> Focus.shift Focus.Up )
        -- down arrow
        40 -> ( state, changesTree |> Focus.shift Focus.Down )
        -- space
        32 ->
            (
                { state
                | hidden = not state.hidden
                }
            , changesTree
            )
            -- executeByPath ()
        -- enter
        13 ->
            case changesTree |> Property.find path of
                Just (Text control) ->
                    let nextProp = (Text <| Control.set ChangeToFire <| Text.finishEditing control)
                    in
                        ( state
                        ,
                            -- FIXME: second time search for a path
                            changesTree
                                |> Property.replaceAt path nextProp
                        -- FIXME: inside, we check if it is a text prop again
                        -- , Tron.perform nextProp
                            --|> Cmd.map (Tuple.pair path)
                        )
                _ -> executeByPath ()
        -- else
        _ -> ( state, changesTree )


expose : State -> Property a -> Property Exp.Out
expose state =
    Property.expose
        >> Property.map
            ( (\val ->
                { update = val
                , client =  Detach.encodeClientId <| Tuple.first <| state.detach
                }
            ) )


expose_ : State -> Property a -> Property ( Exp.Out, a )
expose_ state tree =
    tree
        |> Property.map2 Tuple.pair (tree |> expose state)


collectChanges : State -> Property ( ValueState, Maybe msg ) -> List (Exp.Out, msg)
collectChanges state =
    expose_ state
        >> Property.unfold
        >> List.map (Tuple.second >> Property.get)
        >> List.filterMap
            (\(expOut, (valState, maybeMsg)) ->
                case ( valState, maybeMsg ) of
                    ( ChangeToFire, Just msg ) ->
                        Just <|
                            ( expOut
                            , msg
                            )
                    _ -> Nothing
            )


fireChanges : List change -> Cmd change
fireChanges =
    List.map (Task.succeed >> Task.perform identity)
        >> Cmd.batch


fireChangesFrom : State -> ( Tron msg, Property ValueState ) -> Cmd (Exp.Out, msg)
fireChangesFrom state ( tron, changesTree ) =
    Property.map3
        (\handler currentVal valueState ->
            ( valueState, handler currentVal )
        )
        tron
        (changesTree |> Property.proxify)
        changesTree
            |> collectChanges state
            |> fireChanges


focus : msg -> Cmd msg
focus noOp =
    Dom.focus Layout.rootId
        |> Task.attempt (always noOp)


fromWindow : ((Int, Int) -> msg) -> Cmd msg -- FIXME: get rid of
fromWindow passSize =
    -- Cmd.none
    Dom.getViewport
        |> Task.perform
            (\d -> passSize
                ( floor d.viewport.width
                , floor d.viewport.height
                )
            )


{- Change dock direction of the GUI to any corner of the window, or its center.

See `Style.Dock` for values.
-}
dock : Dock -> State -> State
dock to gui =
    { gui
    | dock = to
    }


{- Set custom shape for all the GUI, in cells. By default, it is calulated from current viewport size, but you may want to reduce the amount of cells, so here's the method. This way GUI will be perfectly in the middle when docked to central positions.
-}
reshape : ( Int, Int ) -> State -> State
reshape cellSize gui =
    { gui
    | size = Just <| Size cellSize
    }


getRootPath : State -> Path
getRootPath gui =
    Tuple.second gui.detach
        |> Detach.stateToMaybe
        |> Maybe.withDefault Path.start


sizeFromViewport : Property a -> Size Pixels -> Size Cells
sizeFromViewport _ (Size ( widthInPixels, heightInPixels )) =
    {- let
        cellsFitHorizontally = floor (toFloat widthInPixels / Cell.width)
        cellsFitVertically = floor (toFloat heightInPixels / Cell.height)
    in -}
        ( floor <| toFloat widthInPixels / Cell.width
        , floor <| toFloat heightInPixels / Cell.height
        ) |> Size



getSizeInCells : State -> Property a -> Size Cells
getSizeInCells state tree =
    case state.size of
        Just userSize -> userSize
        Nothing ->
            state.viewport
                |> sizeFromViewport tree


layout : State -> Property a -> ( Property a, Layout )
layout state tree =
    let
        ( Size cellsSize ) = getSizeInCells state tree
        size = cellsSize |> Tuple.mapBoth toFloat toFloat |> SizeF
    in
    case state.detach
        |> Tuple.second
        |> Detach.stateToMaybe
        |> Maybe.andThen
            (\path ->
                tree
                    |> Property.find path
                    |> Maybe.map (Tuple.pair path)
            ) of
        Nothing ->
            ( tree, Layout.pack state.dock size tree )
        Just ( attachedPath, root ) ->
            ( root, Layout.pack1 state.dock size attachedPath root )


subscriptions : State -> Sub Msg
subscriptions _ =
    Sub.batch
        [ trackMouse
        , trackResize
        , trackSpaceKey
        ]


view : Mode -> Theme -> State -> Property a -> Html Msg
view mode theme state tree  =
    let
        cellsSize = getSizeInCells state tree
        bounds =
            Dock.boundsFromSize state.dock state.viewport cellsSize
        detachState = Tuple.second state.detach
        toDetachAbility =
            case Tuple.first state.detach of
                Just clientId ->
                    Detach.formLocalUrl clientId
                        >> Maybe.map Detach.CanBeDetached
                        >> Maybe.withDefault Detach.CannotBeDetached
                Nothing ->
                    always Detach.CannotBeDetached
    in
    if not state.hidden
    then case layout state tree of
        ( root, theLayout ) ->
            theLayout
                |> Layout.view mode theme state.dock bounds detachState toDetachAbility root
    else Html.div [] []
