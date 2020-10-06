module Gui.Gui exposing
    ( Gui, over, overMap, Flow(..)
    , view, update, init, map
    , trackMouse, focus, reflow, fromWindow
    , detachable
    )


import Browser.Dom as Dom
import Url exposing (Url)
import Task
import Color
import Browser.Events as Browser
import Html exposing (Html)

import BinPack exposing (..)
import Bounds exposing (Bounds)

import Gui.Path exposing (Path)
import Gui.Path as Path exposing (start)
import Gui.Control exposing (..)
import Gui.Control as Control exposing (call)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (call)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (..)
import Gui.Msg exposing (..)
import Gui.Render.Style as Style exposing (..)
import Gui.Render.Layout as Layout exposing (..)
import Gui.Mouse exposing (..)
import Gui.Mouse as Mouse exposing (..)
import Gui.Util exposing (..)
-- import Gui.Alt as Alt exposing (Gui)
import Gui.Focus as Focus exposing (..)
import Gui.Detach as Detach exposing (make, Detach, map)
import Gui.Expose as Exp exposing (..)


type Flow
    = TopToBottom
    | BottomToTop
    | LeftToRight
    | RightToLeft


type alias Gui msg =
    { flow : Flow
    , viewport : ( Int, Int ) -- in pixels
    , size : ( Int, Int ) -- in cells
    , mouse : MouseState
    , tree : Property msg
    , detach : Detach msg
    }


moves : Mouse.Position -> MouseAction
moves = Gui.Mouse.Move
ups : Mouse.Position -> MouseAction
ups = Gui.Mouse.Up
downs : Mouse.Position -> MouseAction
downs = Gui.Mouse.Down


extractMouse : Gui msg -> MouseState
extractMouse = .mouse


map : (msgA -> msgB) -> Gui msgA -> Gui msgB
map f model =
    { flow = model.flow
    , viewport = model.viewport
    , size = model.size
    , mouse = model.mouse
    , tree = model.tree |> Gui.Property.map f
    , detach = model.detach |> Detach.map f
    }


init : Flow -> Gui msg
init flow =
    Gui flow ( -1, -1 ) ( 9, 5 ) Gui.Mouse.init Nil Detach.never


detachable
     : (Exp.RawProperty -> Cmd msg)
    -> (Exp.RawUpdate -> Cmd msg)
    -> Url
    -> Gui msg
    -> Gui msg
detachable sendTree sendUpdate base gui =
    { gui
    | detach = Detach.make sendTree sendUpdate base
    }


over : Property msg -> Gui msg -> Gui msg
over prop gui =
    { gui
    | tree = prop
    }


overMap : (msgA -> msgB) -> Property msgA -> Gui msgB -> Gui msgB
overMap f prop =
    over <| Gui.Property.map f prop


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
                updates =
                    gui.tree |> executeAt path
                nextRoot =
                    gui.tree |> updateMany updates
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , updates |> notifyUpdates gui.detach
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

        Detach path ->
            let
                nextRoot = detachAt path gui.tree
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , Detach.sendTree gui.detach nextRoot
                )

        ReceiveRaw rawUpdate ->
            let
                nextRoot =
                    gui.tree
                        |> Exp.apply (Exp.fromPort rawUpdate)
            in
                (
                    { gui
                    | tree = nextRoot
                    }
                , nextRoot
                    |> Exp.update (Exp.fromPort rawUpdate)
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
reflow viewport gui =
    { gui
    | viewport = viewport
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
        rootPath = getRootPath gui
        curMouseState =
            gui.mouse
        nextMouseState =
            gui.mouse
                |> Gui.Mouse.apply mouseAction
        bounds =
            boundsFromSize gui.viewport gui.size
        layout =
            Layout.pack gui.size gui.tree

        findPathAt pos =
            pos
                |> toGridCoords bounds gui.flow
                |> Layout.find layout

        findCellAt pos =
            pos
                |> findPathAt
                |> Maybe.andThen
                    (\path ->
                        let
                            fullPath = Path.add rootPath path
                        in
                        Gui.Property.find fullPath gui.tree
                            |> Maybe.map (Tuple.pair fullPath)
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
                                satAxis = { min = 0, max = 1, step = 0.01 }
                                curHsla = Color.toHsla curColor
                                ( dX, dY ) = distanceXY knobDistance nextMouseState
                                ( nextHue, nextSaturation ) =
                                    ( alter hueAxis dX curHsla.hue
                                    , alter satAxis dY curHsla.saturation
                                    )
                                nextColor =
                                    Color.hsla
                                        nextHue
                                        nextSaturation
                                        curHsla.lightness
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

                            Just ( path, prop ) ->
                                case prop of
                                    Number _ -> ( path, prop ) |> notifyUpdate gui.detach
                                    Coordinate _ -> ( path, prop ) |> notifyUpdate gui.detach
                                    Color _ -> ( path, prop ) |> notifyUpdate gui.detach
                                    _ -> Cmd.none
                            Nothing -> Cmd.none

                    else Cmd.none
                _ -> Cmd.none

        )


handleKeyDown
    :  Int
    -> Path
    -> Gui msg
    -> ( Gui msg, Cmd msg )
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
                , updates |> notifyUpdates gui.detach
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
        33 -> executeByPath ()
        -- enter
        13 -> executeByPath ()
        -- else
        _ -> ( gui, Cmd.none )


notifyUpdate : Detach msg -> ( Path, Property msg ) -> Cmd msg
notifyUpdate detach ( path, prop ) =
    Cmd.batch
        [ Property.call prop
        , Detach.sendUpdate detach path prop
        ]


notifyUpdates : Detach msg -> List ( Path, Property msg ) -> Cmd msg
notifyUpdates detach =
    List.map
        (notifyUpdate detach)
        >> Cmd.batch


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


boundsFromSize : ( Int, Int ) -> ( Int, Int ) -> Bounds
boundsFromSize ( width, height ) ( gridWidthInCells, gridHeightInCells ) =
    let
        ( gridWidthInPx, gridHeightInPx ) =
            ( cellWidth * toFloat gridWidthInCells
            , cellHeight * toFloat gridHeightInCells
            )
    in
        { x = (toFloat width / 2) - (gridWidthInPx / 2)
        , y = (toFloat height / 2) - (gridHeightInPx / 2)
        , width = gridWidthInPx
        , height = gridHeightInPx
        }


getRootPath : Gui msg -> Path
getRootPath gui =
    gui.detach
        |> Detach.isAttached
        |> Maybe.withDefault Path.start


view : Style.Theme -> Gui msg -> Html Msg
view theme gui =
    let
        bounds =
            boundsFromSize gui.viewport gui.size
    in
    case Detach.isAttached gui.detach
        |> Maybe.andThen
            (\path ->
                gui.tree
                    |> Property.find path
                    |> Maybe.map (Tuple.pair path)
            ) of
        Nothing ->
            Layout.view theme bounds gui.detach gui.tree
                <| Layout.pack gui.size gui.tree
        Just ( attachedPath, root ) ->
            Layout.view theme bounds gui.detach root
                <| Layout.pack1 gui.size attachedPath root
