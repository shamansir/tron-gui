module Gui exposing
    ( Gui
    , view, update, init, subscriptions, run, Msg
    , map, over, use
    , encode, toExposed --, applyRaw, initRaw
    , dock, reshape
    )


{-| `Gui` is the component-like module to be connected with your application.

When you have defined the structure of you GUI using `Gui.Build` module and got the `Builder msg` (where `msg` is the `Msg` of your application) in response, use:

* `init` function to wrap a `Gui msg` over it;
* `subscribe` to make GUI receive all the mouse/keyboard information it requires;
* `update` to pass inner messages to the GUI;
* `view` to render it;

See `example/Basic` in the sources for a full example, here are the important excerpts from it:

    import Gui as Tron

    type Msg = MyMsgOne | MyMsgTwo | ... | ToTron Tron.Msg

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

# Dock & Shape
@docs dock, reshape

# Common Helpers
@docs map, over, use

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

import BinPack exposing (Bounds)
import Size exposing (..)

import Gui.Path exposing (Path)
import Gui.Path as Path exposing (start)
import Gui.Control exposing (..)
import Gui.Control as Control exposing (call)
import Gui.Control.Text as Text exposing (finishEditing)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (call, find)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (..)
import Gui.Msg exposing (..)
import Gui.Render.Layout as Layout exposing (..)
import Gui.Mouse exposing (..)
import Gui.Mouse as Mouse exposing (..)
import Gui.Util exposing (..)
-- import Gui.Alt as Alt exposing (Gui)
import Gui.FocusLogic as Focus exposing (..)
import Gui.Focus as Focus exposing (..)
import Gui.Detach as Detach exposing (ClientId)
import Gui.Expose as Exp exposing (..)
import Gui.Style.Dock exposing (Dock(..))
--import Gui.Style.Anchor exposing (Anchor(..))
import Gui.Style.Dock as Dock exposing (..)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Logic as Style exposing (..)
import Gui.Style.Cell as Cell exposing (..)
import Gui.Build exposing (..)
import Gui.Detach exposing (State(..))


{-| `Gui msg` is what manages your user interface and the way it looks.

`msg` here is the root message type of your application, so that `Gui msg` would be able
to fire all the messages you pass to it in definition. This is similar to how you pass messages in handlers of `Html msg` or `Svg msg`, though in this case `Gui msg` is somewhat a huge component and has its own model and update cycle.

Use `init` to create an instance of `Gui msg`. See the example in the head of the module and `example/` folder for more details.
-}
type alias Gui msg =
    { dock : Dock
    , viewport : Size Pixels
    , size : Maybe (Size Cells)
    , mouse : MouseState
    , tree : Builder msg
    , detach : ( Maybe ClientId, Detach.State )
    }


{-| GUI inner message, similar to the ones in your application.

You don't need it's constructors, only pass it to some `ToTron` wrapping message as in the example above.
-}
type alias Msg = Msg_


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
    { dock = gui.dock
    , viewport = gui.viewport
    , size = gui.size
    , mouse = gui.mouse
    , tree = gui.tree |> Gui.Property.map f
    , detach = gui.detach
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
    ) -> ( Tron.Gui msg, Cmd Tron.Msg )

`dat.gui` doesn't need any side-effects that are produced with `run`, that's why `initRaw` is used there.
-}
initRaw : Builder msg -> Gui msg
initRaw root =
    Gui Dock.topLeft (Size ( 0, 0 )) Nothing Gui.Mouse.init root ( Nothing, Detached )
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


{-| While keeping other options intact and keeping the expanded panels, rebuild the GUI structure using the new model. If some panels were

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
    | tree =
        loadExpanded gui.tree
            |> applyExpanded prop
    }


{-| While keeping other options intact, replace the GUI structure completely.

It is useful both if the model updated externally or you have very different model, and you want to re-build UI using this model, but you don't need/want to notify anyone about the updated values or perform initial effects.

If you have a function like:

    for : MyModel -> Builder MyMsg
    for = ...

..as in the `init` example. Then using `use` in `update` is just:

    gui |> Gui.use (for nextModel)
-}
use : Builder msg -> Gui msg -> Gui msg
use prop gui =
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
    -> Gui msg
    -> ( Gui msg, Cmd ( Path, msg ) )
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


toExposed : Gui msg -> Gui ( RawUpdate, msg )
toExposed gui =
    { dock = gui.dock
    , viewport = gui.viewport
    , size = gui.size
    , mouse = gui.mouse
    , tree = gui.tree |> Exp.toExposed
    , detach = gui.detach
    }


{-
notifyUpdate : Property msg -> Cmd ( msg )
notifyUpdate prop  =
    Property.call prop
    {-
    Cmd.batch
        [ Property.call prop |> Cmd.map (Tuple.pair path)
        , Detach.send detach path prop
        ] -}


notifyUpdates : List ( Property msg ) -> Cmd msg
notifyUpdates =
    List.map notifyUpdate >> Cmd.batch

-}


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


{-| Change dock direction of the GUI to any corner of the window, or its center.

See `Style.Dock` for values.
-}
dock : Dock -> Gui msg -> Gui msg
dock to gui =
    { gui
    | dock = to
    }


{-| Set custom shape for all the GUI, in cells. By default, it is calulated from current viewport size, but you may want to reduce the amount of cells, so here's the method. This way GUI will be perfectly in the middle when docked to central positions.
-}
reshape : ( Int, Int ) -> Gui msg -> Gui msg
reshape cellSize gui =
    { gui
    | size = Just <| Size cellSize
    }


getRootPath : Gui msg -> Path
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



getSizeInCells : Gui msg -> Size Cells
getSizeInCells gui =
    case gui.size of
        Just userSize -> userSize
        Nothing ->
            gui.viewport
                |> sizeFromViewport gui.tree


layout : Gui msg -> ( Property msg, Layout )
layout gui =
    let
        ( Size cellsSize ) = getSizeInCells gui
        size = cellsSize |> Tuple.mapBoth toFloat toFloat
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
        -- , Detach.receive gui.detach -- FIXME: use in `WithGui`
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
view : Theme -> Gui msg -> Html Msg
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
