module Tron.Core exposing
    ( State, Msg
    , view, update, init, subscriptions, run
    , dock, reshape
    , applyRaw
    )


import Browser.Dom as Dom
import Browser.Events as Browser
import Task
import Color
import Html exposing (Html)
import Html.Events as HE
import Json.Encode as E
import Json.Decode as D

import Size exposing (..)

import Tron exposing (Tron)
import Tron.Path exposing (Path)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Control.Text as Text
import Tron.Property exposing (..)
import Tron.Property as Property exposing (run, find)
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
import Tron.Expose as Exp exposing (..)
import Tron.Builder exposing (..)

import Tron.Style.Dock exposing (Dock(..))
import Tron.Style.Dock as Dock exposing (..)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Logic as Style exposing (..)
import Tron.Style.Logic as Dock exposing (boundsFromSize)
import Tron.Style.Cell as Cell exposing (..)

import Tron.Expose.Data as Exp
import Tron.Expose.Convert as Exp
import Tron.Expose.ProxyValue as Exp


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
    -> Tron ( Exp.ProxyValue -> msg )
    -> ( State, Tron (), Cmd msg )
update msg state tree  =
    case msg of
        NoOp ->
            ( state, tree |> invalidate, Cmd.none )

        ApplyMouse mouseAction ->
            handleMouse mouseAction state tree

        Click path ->
            let
                updates =
                    tree |> executeAt path
                nextRoot =
                    tree |> updateMany updates
            in
                ( state
                , nextRoot |> invalidate
                , updates
                    |> List.map (Tuple.second >> Exp.freshRun)
                    |> Cmd.batch
                )

        MouseDown path ->
            ( state
            , Focus.on tree path |> invalidate
            , Cmd.none
            )

        KeyDown keyCode ->
           let
                curFocus = Focus.find tree
            in
                handleKeyDown keyCode curFocus state tree

        ViewportChanged ( w, h ) ->
            (
                { state
                | viewport = Size (w, h)
                }
            , tree |> invalidate
            , Cmd.none
            )

        TextInput path val ->
            let
                nextRoot =
                    tree
                        |> updateTextAt path val
            in
                ( state
                , nextRoot |> invalidate
                , Cmd.none
                )

        Detach path ->
            let
                nextRoot = detachAt path tree
            in
                ( state
                , nextRoot |> invalidate
                , Cmd.none -- FIXME: Detach.sendTree gui.detach nextRoot
                )

        SwitchPage path pageNum ->
            let
                nextRoot = switchPageAt path pageNum tree
            in
                ( state
                , nextRoot |> invalidate
                , Cmd.none
                )



applyRaw
     : Exp.RawInUpdate
    -> Tron (Exp.ProxyValue -> msg)
    -> Cmd msg
applyRaw rawUpdate =
    Exp.apply (Exp.fromPort rawUpdate)
        >> Exp.freshRun


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


-- FIXME: return actual updates with values, then somehow extract messages from `Tron msg` for these values?
handleMouse : MouseAction -> State -> Tron ( Exp.ProxyValue -> msg ) -> ( State, Tron (), Cmd msg )
handleMouse mouseAction state tree =
    let
        rootPath = getRootPath state
        unitTree = tree |> invalidate
        curMouseState =
            state.mouse
        nextMouseState =
            state.mouse
                |> Tron.Mouse.apply mouseAction
        size = getSizeInCells state tree
        bounds =
            Dock.boundsFromSize state.dock state.viewport size
        theLayout =
            layout state unitTree |> Tuple.second

        findPathAt pos =
            pos
                |> Style.toGridCoords bounds
                |> Layout.find theLayout

        findCellAt pos =
            pos
                |> findPathAt
                |> Maybe.andThen
                    (\path ->
                        Tron.Property.find path tree
                            |> Maybe.map (Tuple.pair path)
                    )

    in

        (
            { state
            | mouse = nextMouseState
            }
        , (if curMouseState.down then

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
                            tree

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
                            tree

                Just ( path, Color ( Control state_ curColor handler ) ) ->
                    let
                        hueAxis = { min = 0, max = 1, step = 0.01 }
                        lgtAxis = { min = 0, max = 1, step = 0.01 }
                        curHsla = Color.toHsla curColor
                        ( dX, dY ) = distanceXY knobDistance nextMouseState
                        ( nextHue, nextLightness ) =
                            ( alter hueAxis dX curHsla.hue
                            , alter lgtAxis dY curHsla.lightness
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
                            Control state_ nextColor handler
                    in
                        updateAt
                            path
                            (always <| Color nextControl)
                            tree

                _ ->
                    tree

        else

            nextMouseState.dragFrom
                |> Maybe.andThen findPathAt
                |> Maybe.map (Focus.on tree)
                |> Maybe.withDefault tree)

        |> invalidate

        , case mouseAction of

                Mouse.Up _ ->
                    if curMouseState.down
                    && not nextMouseState.down
                    then

                        case curMouseState.dragFrom |> Maybe.andThen findCellAt of
                        -- TODO: do we need a path returned from `findCellAt`?

                            Just ( _, prop ) ->
                                case prop of
                                    Number _ -> Exp.freshRun prop
                                    Coordinate _ -> Exp.freshRun prop
                                    Color _ -> Exp.freshRun prop
                                    _ -> Cmd.none
                            Nothing -> Cmd.none

                    else Cmd.none
                _ -> Cmd.none

        )



invalidate : Tron a -> Tron ()
invalidate = Tron.map <| always ()


handleKeyDown
    :  Int
    -> Path
    -> State
    -> Tron (Exp.ProxyValue -> msg)
    -> ( State, Tron (), Cmd msg )
handleKeyDown keyCode path state tree =
    let

        executeByPath _ = -- uses only `gui.tree`
            let
                updates = tree |> executeAt path
                nextRoot = tree |> updateMany updates
            in
                ( state
                , nextRoot |> invalidate
                , updates
                    |> List.map (Tuple.second >> Exp.freshRun)
                    |> Cmd.batch
                    --|> Cmd.map (Tuple.pair path)
                )

    in case keyCode of
        -- left arrow
        37 -> ( state, tree |> Focus.shift Focus.Left |> invalidate, Cmd.none )
        -- right arrow
        39 -> ( state, tree |> Focus.shift Focus.Right |> invalidate, Cmd.none )
        -- up arrow
        38 -> ( state, tree |> Focus.shift Focus.Up |> invalidate, Cmd.none )
        -- down arrow
        40 -> ( state, tree |> Focus.shift Focus.Down |> invalidate, Cmd.none )
        -- space
        32 ->
            (
                { state
                | hidden = not state.hidden
                }
            , tree |> invalidate
            , Cmd.none
            )
            -- executeByPath ()
        -- enter
        13 ->
            case tree |> Property.find path of
                Just (Text control) ->
                    let nextProp = (Text <| Text.finishEditing control)
                    in
                        ( state
                        ,
                            -- FIXME: second time search for a path
                            tree
                                |> setAt path nextProp
                                |> invalidate
                        -- FIXME: inside, we check if it is a text prop again
                        , Exp.freshRun nextProp
                            --|> Cmd.map (Tuple.pair path)
                        )
                _ -> executeByPath ()
        -- else
        _ -> ( state, tree |> invalidate, Cmd.none )


{-
toExposed : Model -> Model ( Exp.RawOutUpdate, msg )
toExposed gui =
    { dock = gui.dock
    , viewport = gui.viewport
    , size = gui.size
    , mouse = gui.mouse
    , tree =
        gui.tree
            |> Exp.toExposed
            |> Property.map (Tuple.mapFirst (Detach.addClientId <| Tuple.first <| gui.detach))
    , detach = gui.detach
    , hidden = gui.hidden
    } -}


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



getSizeInCells : State -> Tron a -> Size Cells
getSizeInCells state tree =
    case state.size of
        Just userSize -> userSize
        Nothing ->
            state.viewport
                |> sizeFromViewport tree


layout : State -> Tron a -> ( Tron a, Layout )
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


view : Theme -> State -> Tron a -> Html Msg
view theme state tree  =
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
                |> Layout.view theme state.dock bounds detachState toDetachAbility root
    else Html.div [] []
