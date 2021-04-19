module Tron.Core exposing
    ( Model, Msg
    , view, update, init, subscriptions, run -- FIXME: do not expose
    , map, over, use
    , dock, reshape
    , encode, toExposed
    , applyRaw
    )


import Browser.Dom as Dom
import Browser.Events as Browser
import Task
import Color
import Html exposing (Html)
import Json.Encode as E

import Size exposing (..)

import Tron exposing (Tron)
import Tron.Path exposing (Path)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Control.Text as Text
import Tron.Property exposing (..)
import Tron.Property as Property exposing (call, find)
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


type alias Model msg =
    { dock : Dock
    , viewport : Size Pixels
    , size : Maybe (Size Cells)
    , mouse : MouseState
    , tree : Tron msg
    , detach : ( Maybe ClientId, Detach.State )
    }


type alias Msg = Msg_


moves : Mouse.Position -> MouseAction
moves = Tron.Mouse.Move
ups : Mouse.Position -> MouseAction
ups = Tron.Mouse.Up
downs : Mouse.Position -> MouseAction
downs = Tron.Mouse.Down


extractMouse : Model msg -> MouseState
extractMouse = .mouse


map : (msgA -> msgB) -> Model msgA -> Model msgB
map f gui =
    { dock = gui.dock
    , viewport = gui.viewport
    , size = gui.size
    , mouse = gui.mouse
    , tree = gui.tree |> Tron.Property.map f
    , detach = gui.detach
    }


init : Tron msg -> ( Model msg, Cmd Msg )
init root =
    ( initRaw root
    , run
    )


initRaw : Tron msg -> Model msg
initRaw root =
    Model Dock.topLeft (Size ( 0, 0 )) Nothing Tron.Mouse.init root ( Nothing, Detached )
-- TODO: get rid of initRaw


run : Cmd Msg
run =
    Cmd.batch
        [ focus NoOp
        , fromWindow ViewportChanged
        ]


{- While keeping other options intact and keeping the expanded panels, rebuild the GUI structure using the new model. If some panels were

It is useful if the model updated externally, you want to re-build UI using this model,
but you don't need/want to notify anyone about the updated values or perform initial effects.

If you have a function like:

    for : MyModel -> Builder MyMsg
    for = ...

..as in the `init` example. Then using `over` in `update` is just:

    gui |> Tron.over (for nextModel)
-}
over : Tron msg -> Model msg -> Model msg
over prop gui =
    let
        lastFocus = Focus.find gui.tree
    in
        { gui
        | tree =
            transferTransientState gui.tree prop --<| Focus.on prop lastFocus
            -- loadTransientState gui.tree
            --     |> applyTransientState (Focus.on prop lastFocus)
        }


{- While keeping other options intact, replace the GUI structure completely.

It is useful both if the model updated externally or you have very different model, and you want to re-build UI using this model, but you don't need/want to notify anyone about the updated values or perform initial effects.

If you have a function like:

    for : MyModel -> Builder MyMsg
    for = ...

..as in the `init` example. Then using `use` in `update` is just:

    gui |> Tron.use (for nextModel)
-}
use : Tron msg -> Model msg -> Model msg
use prop gui =
    { gui
    | tree = prop
    }


{- Encode any Tron GUI structure into JSON.

That allows you to re-create one from WebSockets or to build the same GUI
in `dat.gui` and gives many other possibilities.
-}
encode : Model msg -> E.Value
encode = .tree >> Exp.encode


-- overMap : (msgA -> msgB) -> Property msgA -> Model msgB -> Model msgB
-- overMap f prop =
--     over <| Tron.Property.map f prop


update
    :  Msg
    -> Model msg
    -> ( Model msg, Cmd msg )
update msg gui =
    case msg of
        NoOp ->
            ( gui, Cmd.none )

        ApplyMouse mouseAction ->
            handleMouse mouseAction gui

        Click path ->
            let
                updates =
                    gui.tree |> executeAt path
                nextRoot =
                    gui.tree |> updateMany updates
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , updates
                    |> List.map (Tuple.second >> Property.call)
                    |> Cmd.batch
                )

        MouseDown path ->
            (
                { gui
                | tree = Focus.on gui.tree path
                }
            , Cmd.none
            )

        KeyDown keyCode ->
           let
                curFocus = Focus.find gui.tree
            in
                handleKeyDown keyCode curFocus gui
                    |> Tuple.mapSecond (Cmd.map Tuple.second)

        ViewportChanged ( w, h ) ->
            (
                { gui
                | viewport = Size (w, h)
                }
            , Cmd.none
            )

        TextInput path val ->
            let
                nextRoot =
                    gui.tree
                        |> updateTextAt path val
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , Cmd.none
                )

        Detach path ->
            let
                nextRoot = detachAt path gui.tree
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , Cmd.none -- FIXME: Detach.sendTree gui.detach nextRoot
                )

        SwitchPage path pageNum ->
            let
                nextRoot = switchPageAt path pageNum gui.tree
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , Cmd.none
                )


applyRaw
     : Exp.RawInUpdate
    -> Model msg
    -> Cmd msg
applyRaw rawUpdate =
    .tree >> Exp.update (Exp.fromPort rawUpdate)


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


handleMouse : MouseAction -> Model msg -> ( Model msg, Cmd msg )
handleMouse mouseAction gui =
    let
        rootPath = getRootPath gui
        curMouseState =
            gui.mouse
        nextMouseState =
            gui.mouse
                |> Tron.Mouse.apply mouseAction
        size = getSizeInCells gui
        bounds =
            Dock.boundsFromSize gui.dock gui.viewport size
        theLayout =
            layout gui |> Tuple.second

        findPathAt pos =
            pos
                |> Style.toGridCoords bounds
                |> Layout.find theLayout

        findCellAt pos =
            pos
                |> findPathAt
                |> Maybe.andThen
                    (\path ->
                        Tron.Property.find path gui.tree
                            |> Maybe.map (Tuple.pair path)
                    )

    in

        (
            { gui
            | mouse = nextMouseState
            , tree =

                if curMouseState.down then

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

                        Just ( path, Color ( Control state curColor handler ) ) ->
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
                                    Control state nextColor handler
                            in
                                updateAt
                                    path
                                    (always <| Color nextControl)
                                    gui.tree

                        _ ->
                            gui.tree

                else

                    nextMouseState.dragFrom
                        |> Maybe.andThen findPathAt
                        |> Maybe.map (Focus.on gui.tree)
                        |> Maybe.withDefault gui.tree

            }

        ,

            case mouseAction of

                Mouse.Up _ ->
                    if curMouseState.down
                    && not nextMouseState.down
                    then

                        case curMouseState.dragFrom |> Maybe.andThen findCellAt of
                        -- TODO: do we need a path returned from `findCellAt`?

                            Just ( _, prop ) ->
                                case prop of
                                    Number _ -> Property.call prop
                                    Coordinate _ -> Property.call prop
                                    Color _ -> Property.call prop
                                    _ -> Cmd.none
                            Nothing -> Cmd.none

                    else Cmd.none
                _ -> Cmd.none

        )


handleKeyDown
    :  Int
    -> Path
    -> Model msg
    -> ( Model msg, Cmd ( Path, msg ) )
handleKeyDown keyCode path gui =
    let

        shiftFocus to =
            let
                nextRoot = gui.tree |> Focus.shift to
            in
                { gui
                | tree = nextRoot
                }

        executeByPath _ =
            let
                updates = gui.tree |> executeAt path
                nextRoot = gui.tree |> updateMany updates
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , updates
                    |> List.map (Tuple.second >> Property.call)
                    |> Cmd.batch
                    |> Cmd.map (Tuple.pair path)
                )

    in case keyCode of
        -- left arrow
        37 -> ( shiftFocus Focus.Left, Cmd.none )
        -- right arrow
        39 -> ( shiftFocus Focus.Right, Cmd.none )
        -- up arrow
        38 -> ( shiftFocus Focus.Up, Cmd.none )
        -- down arrow
        40 -> ( shiftFocus Focus.Down, Cmd.none )
        -- space
        33 -> executeByPath gui.tree
        -- enter
        13 ->
            case gui.tree |> Property.find path of
                Just (Text control) ->
                    let nextProp = (Text <| Text.finishEditing control)
                    in
                        (
                            { gui
                            | tree =
                                -- FIXME: second time search for a path
                                gui.tree
                                    |> setAt path nextProp
                            }
                        -- FIXME: inside, we check if it is a text prop again
                        , Property.call nextProp
                            |> Cmd.map (Tuple.pair path)
                        )
                _ -> executeByPath ()
        -- else
        _ -> ( gui, Cmd.none )


toExposed : Model msg -> Model ( Exp.RawOutUpdate, msg )
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
    }


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
dock : Dock -> Model msg -> Model msg
dock to gui =
    { gui
    | dock = to
    }


{- Set custom shape for all the GUI, in cells. By default, it is calulated from current viewport size, but you may want to reduce the amount of cells, so here's the method. This way GUI will be perfectly in the middle when docked to central positions.
-}
reshape : ( Int, Int ) -> Model msg -> Model msg
reshape cellSize gui =
    { gui
    | size = Just <| Size cellSize
    }


getRootPath : Model msg -> Path
getRootPath gui =
    Tuple.second gui.detach
        |> Detach.stateToMaybe
        |> Maybe.withDefault Path.start


sizeFromViewport : Property msg -> Size Pixels -> Size Cells
sizeFromViewport _ (Size ( widthInPixels, heightInPixels )) =
    {- let
        cellsFitHorizontally = floor (toFloat widthInPixels / Cell.width)
        cellsFitVertically = floor (toFloat heightInPixels / Cell.height)
    in -}
        ( floor <| toFloat widthInPixels / Cell.width
        , floor <| toFloat heightInPixels / Cell.height
        ) |> Size



getSizeInCells : Model msg -> Size Cells
getSizeInCells gui =
    case gui.size of
        Just userSize -> userSize
        Nothing ->
            gui.viewport
                |> sizeFromViewport gui.tree


layout : Model msg -> ( Property msg, Layout )
layout gui =
    let
        ( Size cellsSize ) = getSizeInCells gui
        size = cellsSize |> Tuple.mapBoth toFloat toFloat |> SizeF
    in
    case gui.detach
        |> Tuple.second
        |> Detach.stateToMaybe
        |> Maybe.andThen
            (\path ->
                gui.tree
                    |> Property.find path
                    |> Maybe.map (Tuple.pair path)
            ) of
        Nothing ->
            ( gui.tree, Layout.pack gui.dock size gui.tree )
        Just ( attachedPath, root ) ->
            ( root, Layout.pack1 gui.dock size attachedPath root )


subscriptions : Model msg -> Sub Msg
subscriptions gui =
    Sub.batch
        [ trackMouse
        -- , Detach.receive gui.detach -- FIXME: use in `WithTron`
        , Browser.onResize <| \w h -> ViewportChanged ( w, h )
        ]


view : Theme -> Model msg -> Html Msg
view theme gui =
    let
        cellsSize = getSizeInCells gui
        bounds =
            Dock.boundsFromSize gui.dock gui.viewport cellsSize
        detachState = Tuple.second gui.detach
        toDetachAbility =
            case Tuple.first gui.detach of
                Just clientId ->
                    Detach.formLocalUrl clientId
                        >> Maybe.map Detach.CanBeDetached
                        >> Maybe.withDefault Detach.CannotBeDetached
                Nothing ->
                    always Detach.CannotBeDetached
    in
    case layout gui of
        ( root, theLayout ) ->
            theLayout
                |> Layout.view theme gui.dock bounds detachState toDetachAbility root
