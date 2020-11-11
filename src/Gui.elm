module Gui exposing
    ( Gui
    , view, update, init, subscriptions, run, Message
    , map, over
    , detachable, encode, applyRaw, initRaw
    , reflow
    )


{-| `Gui` is the component-like module to be connected with your application.

When you have defined the structure of you GUI using `Gui.Build` module and got the `Builder msg` (where `msg` is the `Msg` of your application) in response, use:

* `init` function to wrap a `Gui msg` over it;
* `subscribe` to make GUI receive all the mouse/keyboard information it requires;
* `update` to pass inner messages to the GUI;
* `view` to render it;

See `example/Basic` in the sources for a full example, here are the important excerpts from it:

    import Gui as Tron

    type Msg = MyMsgOne | MyMsgTwo | ... | ToTron Tron.Message

    init _ =
        let
            myModel = MyModel.init -- init your model
            ( gui, guiEffect ) =
                MyGui.for myModel -- create a `Builder msg` from your model
                    |> Tron.init -- and immediately create the GUI
        in
            (
                ( myModel
                , gui -- store GUI in your model, as one would do with a component model
                )
            , guiEffect |> Cmd.map ToTron -- map the messages of GUI to itself
            )

    view ( myModel, gui ) =
        Html.div [ ]
            [ gui
                |> Tron.view Tron.Light -- Light vs. Dark theme
                |> Html.map ToTron
            , MyApp.view myModel
            ]

    update msg ( myModel, gui ) =
        case msg of
            MyMsgOne -> ...
            MyMsgTwo -> ...
            ... -> ...
            ToTron guiMsg ->
                case gui |> Gui.update guiMsg of
                    ( nextGui, cmds ) ->
                        ( ( myModel, nextGui )
                        , cmds
                        )

    subscriptions ( _, gui ) =
        Tron.subscriptions gui |> Sub.map ToTron

That's enough to make your application work with Tron!

If you need features that exceed Basic functionality like detachable parts or communication with JS, they can be purchased in the store. It's a joke, just lead to another examples in the `example` folder and to the `Gui.Detach` module documentation.

For controlling the way GUI looks, see `Gui.Render.Style` module.

NB: Don't forget to copy `src/Gui.css` to your application to make GUI look and behave properly.

# Core
@docs Gui

# Lifecycle
@docs init, update, view, subscriptions, run, Message

# Flow
@docs reflow

# Common Helpers
@docs map, over

# Detaching and Connecting to JavaScript
@docs detachable, encode, applyRaw, initRaw

-}


import Browser.Dom as Dom
import Url exposing (Url)
import Task
import Color
import Browser.Events as Browser
import Html exposing (Html)
import Json.Encode as E

import BinPack exposing (..)
import Bounds exposing (Bounds)

import Gui.Path exposing (Path)
import Gui.Path as Path exposing (start)
import Gui.Control exposing (..)
import Gui.Control as Control exposing (call)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (call, find)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (..)
import Gui.Msg exposing (..)
import Gui.Render.Style as Style exposing (..)
import Gui.Render.StyleLogic as Style exposing (..)
import Gui.Render.Layout as Layout exposing (..)
import Gui.Mouse exposing (..)
import Gui.Mouse as Mouse exposing (..)
import Gui.Util exposing (..)
-- import Gui.Alt as Alt exposing (Gui)
import Gui.Focus as Focus exposing (..)
import Gui.Detach as Detach exposing (make, ClientId, Detach, map)
import Gui.Expose as Exp exposing (..)
import Gui.Build exposing (..)


{-| `Gui msg` is what manages your user interface and the way it looks.

`msg` here is the root message type of your application, so that `Gui msg` would be able
to fire all the messages you pass to it in definition. This is similar to how you pass messages in handlers of `Html msg` or `Svg msg`, though in this case `Gui msg` is somewhat a huge component and has its own model and update cycle.

Use `init` to create an instance of `Gui msg`. See the example in the head of the module and `example/` folder for more details.
-}
type alias Gui msg =
    { flow : Flow
    , viewport : ( Int, Int )
    , mouse : MouseState
    , tree : Builder msg
    , detach : Detach msg
    }


{-| GUI inner message, similar to the ones in your application.

You don't need it's constructors, only pass it to some `ToTron` wrapping message as in the example above.
-}
type alias Message = Msg


moves : Mouse.Position -> MouseAction
moves = Gui.Mouse.Move
ups : Mouse.Position -> MouseAction
ups = Gui.Mouse.Up
downs : Mouse.Position -> MouseAction
downs = Gui.Mouse.Down


extractMouse : Gui msg -> MouseState
extractMouse = .mouse


{-| Make the same Tron GUI work with other messages, since you provide
the conversion function.
-}
map : (msgA -> msgB) -> Gui msgA -> Gui msgB
map f gui =
    { flow = gui.flow
    , viewport = gui.viewport
    , mouse = gui.mouse
    , tree = gui.tree |> Gui.Property.map f
    , detach = gui.detach |> Detach.map f
    }


{-| Initialize Tron from `Builder msg`. See `Gui.Build` module for documentation on how
to build your GUI from the model, usually it is something like:

    import Gui exposing (Gui)
    import Gui.Build as Builder exposing (..)

    type Msg = MyMsgOne Int | MyMsgTwo Bool | ... | ToTron Gui.Message

    for : MyModel -> Builder MyMsg
    for myModel =
        [ ( "int", Builder.int ... MyMsgOne ... )
        , ( "toggle", Builder.toggle ... MyMsgTwo ... )
        , ...
        ]

    init : flags -> ( ( MyModel, Gui MyMsg ), Cmd MyMsg )
    init _ =
        let
            myModel = MyModel.init -- init your model
            ( gui, guiEffect ) =
                for myModel -- create a `Builder MyMsg` from your model
                    |> Gui.init -- and immediately create the GUI
        in
            (
                ( myModel
                , gui -- store GUI in your model, as one would do with a component model
                )
            , guiEffect |> Cmd.map ToTron -- map the messages of GUI to itself
            )

Tron GUI needs some side-effect initialization, like do a keyboard focus or get the current window size, that's why it also produces `Cmd Gui.Message`. Feel free to `Cmd.batch` it with your effects.
-}
init : Builder msg -> ( Gui msg, Cmd Msg )
init root =
    ( initRaw root
    , run
    )


{-| `initRaw` is needed only for the cases of replacing Tron interface with `dat.gui` or any other JS interpretation. See `example/DatGui` for reference.

Since `init builder` is just:

    ( initRaw builder
    , run
    ) -> ( Tron.Gui msg, Cmd Tron.Message )

`dat.gui` doesn't need any side-effects that are produced with `run`, that's why `initRaw` is used there.
-}
initRaw : Builder msg -> Gui msg
initRaw root =
    Gui TopToBottom ( -1, -1 ) Gui.Mouse.init root Detach.never
-- TODO: get rid of initRaw


{-| Perform the effects needed for initialization. Call it if you don't use the visual part of Tron (i.e. for `dat.gui`) or when you re-create the GUI.

Tron GUI needs some side-effect initialization, like do a keyboard focus or get the current window size, that's why it produces `Cmd Gui.Message`.
-}
run : Cmd Msg
run =
    Cmd.batch
        [ focus NoOp
        , fromWindow ViewportChanged
        ]


{-| This is the only function you need to make your `GUI` _detachable*_. However, this function requires some ports to be present as an argument, so you'll need a pair of ports as well. And a WebSocket server. But that's it!

_*_ — _detachable GUI_ in the context of Web Application means that you may move parts of your user interface to another browser window, tab, or even another device, such as a phone, a tablet, TV, VR glasses or whatever has a browser inside nowadays.

For a detailed example, see `example/Detachable` in the sources.
-}
detachable
     : Url
    -> (Exp.Ack -> Cmd msg)
    -> (Exp.RawUpdate -> Cmd msg)
    -> ((Exp.RawUpdate -> Msg) -> Sub Msg)
    -> Gui msg
    -> ( Gui msg, Cmd Msg )
detachable url ack send receive gui =
    let
        ( detach, detachEffects )
            = Detach.make url ack send receive
    in
        (
            { gui
            | detach = detach
            }
        , detachEffects
        )


{-| While keeping other options intact, replace the GUI structure completely.

It is useful if the model updated externally, you want to re-build UI using this model,
but you don't need/want to notify anyone about the updated values or perform initial effects.

If you have a function like:

    for : MyModel -> Builder MyMsg
    for = ...

..as in the `init` example. Then using `over` in `update` is just:

    gui |> Gui.over (for nextModel)
-}
over : Builder msg -> Gui msg -> Gui msg
over prop gui =
    { gui
    | tree = prop
    }


{-| Encode any Tron GUI structure into JSON.

That allows you to re-create one from WebSockets or to build the same GUI
in `dat.gui` and gives many other possibilities.
-}
encode : Gui msg -> E.Value
encode = .tree >> Exp.encode


-- overMap : (msgA -> msgB) -> Property msgA -> Gui msgB -> Gui msgB
-- overMap f prop =
--     over <| Gui.Property.map f prop


{-| The usual `update` function, but for `Gui msg` (where `msg` is your message in your application), and it consumes `Gui.Message`.

Install it into your `update` function similarly to:

    type Msg = MyMsgOne | MyMsgTwo | ... | ToTron Gui.Message

    update msg ( myModel, gui ) =
        case msg of
            MyMsgOne -> ...
            MyMsgTwo -> ...
            ... -> ...
            ToTron guiMsg ->
                case gui |> Gui.update guiMsg of
                    ( nextGui, cmds ) ->
                        ( ( myModel, nextGui )
                        , cmds
                        )
-}
update
    :  Msg
    -> Gui msg
    -> ( Gui msg, Cmd msg )
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

        ViewportChanged viewport ->
            (
                { gui
                | viewport = viewport
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
                , Cmd.none -- Detach.sendTree gui.detach nextRoot
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

        SetClientId clientId ->
            let
                nextDetach =
                    gui.detach
                        |> Detach.setClientId clientId
            in
                (
                    { gui
                    | detach = nextDetach

                    }
                , nextDetach |> Detach.ack
                )


{-| `applyRaw` is needed only for the cases of replacing Tron interface with `dat.gui` or any other JS interpretation. See `example/DatGui` for reference.

It receives the RAW value update (from port in JSON format, for example) and applies it to the GUI so that the proper user message is fired from the handler.
-}
applyRaw
     : Exp.RawUpdate
    -> Gui msg
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


handleMouse : MouseAction -> Gui msg -> ( Gui msg, Cmd msg )
handleMouse mouseAction gui =
    let
        rootPath = getRootPath gui
        curMouseState =
            gui.mouse
        nextMouseState =
            gui.mouse
                |> Gui.Mouse.apply mouseAction
        size =
            gui.viewport
                |> sizeFromViewport gui.tree
        bounds =
            boundsFromSize gui.viewport size
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
        33 -> executeByPath gui.tree
        -- enter
        13 ->
            case gui.tree |> Property.find path of
                Just (Text control) ->
                    let nextProp = (Text <| finishEditing control)
                    in
                        (
                            { gui
                            | tree =
                                -- FIXME: second time search for a path
                                gui.tree
                                    |> setAt path nextProp
                            }
                        -- FIXME: inside, we check if it is a text prop again
                        , notifyUpdate gui.detach ( path, nextProp )
                        )
                _ -> executeByPath ()
        -- else
        _ -> ( gui, Cmd.none )


notifyUpdate : Detach msg -> ( Path, Property msg ) -> Cmd msg
notifyUpdate detach ( path, prop ) =
    Cmd.batch
        [ Property.call prop
        , Detach.send detach path prop
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


getViewport : Gui msg -> ( Int, Int )
getViewport = .viewport


{-| Change flow direction of the GUI to `TopToBottom`, `BottomToTop`, `RightToLeft` or `LeftToRight`.

See `Style.Flow` for values.
-}
reflow : Flow -> Gui msg -> Gui msg
reflow flow gui =
    { gui
    | flow = flow
    }


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
    Detach.isAttached gui.detach
        |> Maybe.withDefault Path.start


sizeFromViewport : Property msg -> ( Int, Int ) -> ( Int, Int )
sizeFromViewport root ( widthInPixels, heightInPixels ) =
    let
        cellsFitHorizontally = floor <| toFloat widthInPixels / (cellWidth + gap)
        cellsFitVertically = floor <| toFloat heightInPixels / (cellHeight + gap)
    in
        ( if cellsFitHorizontally > 3 then cellsFitHorizontally - 3 else cellsFitHorizontally
        , if cellsFitVertically > 2 then cellsFitVertically - 2 else cellsFitVertically
        )


layout : Gui msg -> ( Property msg, Layout )
layout gui =
    let
        size =
            gui.viewport
                |> sizeFromViewport gui.tree
    in
    case Detach.isAttached gui.detach
        |> Maybe.andThen
            (\path ->
                gui.tree
                    |> Property.find path
                    |> Maybe.map (Tuple.pair path)
            ) of
        Nothing ->
            ( gui.tree, Layout.pack gui.flow size gui.tree )
        Just ( attachedPath, root ) ->
            ( root, Layout.pack1 gui.flow size attachedPath root )


{-| Subscribe the updates of the GUI, so it would resize with the window,
track mouse etc.

`Sub.map` it to the message that will wrap `Gui.Message` and send it to `update`:

    subscriptions ( myModel, gui ) =
        Sub.batch
            [ ...
            , Gui.subscribe gui |> Sub.map ToTron
            ]
-}
subscriptions : Gui msg -> Sub Msg
subscriptions gui =
    Sub.batch
        [ trackMouse
        , Detach.receive gui.detach
        , Browser.onResize <| \w h -> ViewportChanged ( w, h )
        ]


{-| Build the corresponding structure for Tron GUI.

Use it in your `view` function, just `Html.map` it to the message
that will wrap `Gui.Message` and send it to `update`:

    view ( myModel, gui ) =
        Html.div [ ]
            [ gui
                |> Gui.view Tron.Light
                |> Html.map ToTron
            , MyApp.view myModel
            ]

Use `Theme` from `Gui.Style` to set it to `Dark` or `Light` theme.
-}
view : Style.Theme -> Gui msg -> Html Msg
view theme gui =
    let
        size =
            gui.viewport
                |> sizeFromViewport gui.tree
        bounds =
            boundsFromSize gui.viewport size
    in
    case layout gui of
        ( root, theLayout ) ->
            theLayout
                |> Layout.view theme gui.flow bounds gui.detach root
