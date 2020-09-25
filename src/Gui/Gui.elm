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
import Gui.Control exposing (..)
import Gui.Property exposing (..)
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
import Gui.Detach as Detach exposing (..)


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
    , layout : Layout -- seems to be updated after every tree change, so don't store?
    , detach : DetachFn
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
    , detach = model.detach
    }


init : Flow -> Gui msg
init flow =
    Gui flow Bounds.zero Gui.Mouse.init Nil Layout.init Detach.never


detachBy : (Path -> Maybe Url) -> Gui msg -> Gui msg
detachBy detachFn gui =
    { gui
    | detach = DetachFn detachFn
    }


detachable : Url -> Gui msg -> Gui msg
detachable base gui =
    { gui
    | detach = Detach.default base
    }


over : Property msg -> Gui msg -> Gui msg
over prop gui =
    { gui
    | tree = prop
    , layout = Layout.pack prop
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
                | tree = Focus.on gui.tree path
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

        Detach path ->
            let
                nextRoot = detachAt path gui.tree
            in
                (
                    { gui
                    | tree = nextRoot
                    , layout = Layout.pack nextRoot
                    }
                , Cmd.none
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

        findPathAt pos =
            pos
                |> toGridCoords gui.bounds gui.flow
                |> Layout.find gui.layout

        findCellAt pos =
            pos
                |> findPathAt
                |> Maybe.andThen
                    (\path ->
                        Gui.Property.find path gui.tree
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

                            Just ( _, Number control ) ->
                                call control
                            Just ( _, Coordinate control ) ->
                                call control
                            Just ( _, Color control ) ->
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


view : Style.Theme -> Gui msg -> Html Msg
view theme gui =
    Layout.view theme gui.bounds gui.detach gui.tree gui.layout
