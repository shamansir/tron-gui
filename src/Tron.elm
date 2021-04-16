module Tron exposing
    ( Tron, Msg
    , view, update, init, subscriptions, run -- FIXME: do not expose
    , map, over, use
    , dock, reshape
    , encode, toExposed
    , applyRaw
    )


{-| `Tron` is the component-like module to be connected with your application.

When you have defined the structure of you GUI using `Tron.Builder` module and got the `Builder msg` (where `msg` is the `Msg` of your application) in response, use:

* `init` function to wrap a `Tron msg` over it;
* `subscribe` to make GUI receive all the mouse/keyboard information it requires;
* `update` to pass inner messages to the GUI;
* `view` to render it;

See `example/Basic` in the sources for a full example, here are the important excerpts from it:

    import Tron

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
                case gui |> Tron.update guiMsg of
                    ( nextGui, cmds ) ->
                        ( ( myModel, nextGui )
                        , cmds
                        )

    subscriptions ( _, gui ) =
        Tron.subscriptions gui |> Sub.map ToTron

That's enough to make your application work with Tron!

If you need features that exceed Basic functionality like detachable parts or communication with JS, they can be purchased in the store. It's a joke, just lead to another examples in the `example` folder and to the `Tron.Detach` module documentation.

For controlling the way GUI looks, see `Tron.Render.Style` module.

NB: Don't forget to copy `src/Tron.css` to your application to make GUI look and behave properly.

# Core
@docs Tron

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
import Dict exposing (Dict)

import BinPack exposing (Bounds)
import Size exposing (..)

import Tron.Path exposing (Path)
import Tron.Path as Path exposing (start)
import Tron.Control exposing (..)
import Tron.Control as Control exposing (call)
import Tron.Control.Text as Text exposing (finishEditing)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (call, find)
import Tron.Layout exposing (Layout)
import Tron.Layout as Layout exposing (..)
import Tron.Msg exposing (..)
import Tron.Render.Layout as Layout exposing (..)
import Tron.Mouse exposing (..)
import Tron.Mouse as Mouse exposing (..)
import Tron.Util exposing (..)
-- import Tron.Alt as Alt exposing (Tron)
import Tron.FocusLogic as Focus exposing (..)
import Tron.Focus as Focus exposing (..)
import Tron.Detach as Detach exposing (ClientId)
import Tron.Expose as Exp exposing (..)
import Tron.Style.Dock exposing (Dock(..))
--import Tron.Style.Anchor exposing (Anchor(..))
import Tron.Style.Dock as Dock exposing (..)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Logic as Style exposing (..)
import Tron.Style.Cell as Cell exposing (..)
import Tron.Builder exposing (..)
import Tron.Detach exposing (State(..))


{-| `Tron msg` is what manages your user interface and the way it looks.

`msg` here is the root message type of your application, so that `Tron msg` would be able
to fire all the messages you pass to it in definition. This is similar to how you pass messages in handlers of `Html msg` or `Svg msg`, though in this case `Tron msg` is somewhat a huge component and has its own model and update cycle.

Use `init` to create an instance of `Tron msg`. See the example in the head of the module and `example/` folder for more details.
-}
type alias Tron msg =
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
moves = Tron.Mouse.Move
ups : Mouse.Position -> MouseAction
ups = Tron.Mouse.Up
downs : Mouse.Position -> MouseAction
downs = Tron.Mouse.Down


extractMouse : Tron msg -> MouseState
extractMouse = .mouse


{-| Make the same Tron GUI work with other messages, since you provide
the conversion function.
-}
map : (msgA -> msgB) -> Tron msgA -> Tron msgB
map f gui =
    { dock = gui.dock
    , viewport = gui.viewport
    , size = gui.size
    , mouse = gui.mouse
    , tree = gui.tree |> Tron.Property.map f
    , detach = gui.detach
    }


{-| Initialize Tron from `Builder msg`. See `Tron.Builder` module for documentation on how
to build your GUI from the model, usually it is something like:

    import Tron exposing (Tron)
    import Tron.Builder as Builder exposing (..)

    type Msg = MyMsgOne Int | MyMsgTwo Bool | ... | ToTron Tron.Message

    for : MyModel -> Builder MyMsg
    for myModel =
        [ ( "int", Builder.int ... MyMsgOne ... )
        , ( "toggle", Builder.toggle ... MyMsgTwo ... )
        , ...
        ]

    init : flags -> ( ( MyModel, Tron MyMsg ), Cmd MyMsg )
    init _ =
        let
            myModel = MyModel.init -- init your model
            ( gui, guiEffect ) =
                for myModel -- create a `Builder MyMsg` from your model
                    |> Tron.init -- and immediately create the GUI
        in
            (
                ( myModel
                , gui -- store GUI in your model, as one would do with a component model
                )
            , guiEffect |> Cmd.map ToTron -- map the messages of GUI to itself
            )

Tron GUI needs some side-effect initialization, like do a keyboard focus or get the current window size, that's why it also produces `Cmd Tron.Message`. Feel free to `Cmd.batch` it with your effects.
-}
init : Builder msg -> ( Tron msg, Cmd Msg )
init root =
    ( initRaw root
    , run
    )


{-| `initRaw` is needed only for the cases of replacing Tron interface with `dat.gui` or any other JS interpretation. See `example/DatGui` for reference.

Since `init builder` is just:

    ( initRaw builder
    , run
    ) -> ( Tron.Tron msg, Cmd Tron.Msg )

`dat.gui` doesn't need any side-effects that are produced with `run`, that's why `initRaw` is used there.
-}
initRaw : Builder msg -> Tron msg
initRaw root =
    Tron Dock.topLeft (Size ( 0, 0 )) Nothing Tron.Mouse.init root ( Nothing, Detached )
-- TODO: get rid of initRaw


{-| Perform the effects needed for initialization. Call it if you don't use the visual part of Tron (i.e. for `dat.gui`) or when you re-create the GUI.

Tron GUI needs some side-effect initialization, like do a keyboard focus or get the current window size, that's why it produces `Cmd Tron.Message`.
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

    gui |> Tron.over (for nextModel)
-}
over : Builder msg -> Tron msg -> Tron msg
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


{-| While keeping other options intact, replace the GUI structure completely.

It is useful both if the model updated externally or you have very different model, and you want to re-build UI using this model, but you don't need/want to notify anyone about the updated values or perform initial effects.

If you have a function like:

    for : MyModel -> Builder MyMsg
    for = ...

..as in the `init` example. Then using `use` in `update` is just:

    gui |> Tron.use (for nextModel)
-}
use : Builder msg -> Tron msg -> Tron msg
use prop gui =
    { gui
    | tree = prop
    }


{-| Encode any Tron GUI structure into JSON.

That allows you to re-create one from WebSockets or to build the same GUI
in `dat.gui` and gives many other possibilities.
-}
encode : Tron msg -> E.Value
encode = .tree >> Exp.encode


-- overMap : (msgA -> msgB) -> Property msgA -> Tron msgB -> Tron msgB
-- overMap f prop =
--     over <| Tron.Property.map f prop


{-| The usual `update` function, but for `Tron msg` (where `msg` is your message in your application), and it consumes `Tron.Message`.

Install it into your `update` function similarly to:

    type Msg = MyMsgOne | MyMsgTwo | ... | ToTron Tron.Message

    update msg ( myModel, gui ) =
        case msg of
            MyMsgOne -> ...
            MyMsgTwo -> ...
            ... -> ...
            ToTron guiMsg ->
                case gui |> Tron.update guiMsg of
                    ( nextGui, cmds ) ->
                        ( ( myModel, nextGui )
                        , cmds
                        )
-}
update
    :  Msg
    -> Tron msg
    -> ( Tron msg, Cmd msg )
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


{-| `applyRaw` is needed only for the cases of replacing Tron interface with `dat.gui` or any other JS interpretation. See `example/DatGui` for reference.

It receives the RAW value update (from port in JSON format, for example) and applies it to the GUI so that the proper user message is fired from the handler.
-}
applyRaw
     : Exp.RawInUpdate
    -> Tron msg
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


handleMouse : MouseAction -> Tron msg -> ( Tron msg, Cmd msg )
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
    -> Tron msg
    -> ( Tron msg, Cmd ( Path, msg ) )
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


toExposed : Tron msg -> Tron ( RawOutUpdate, msg )
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
dock : Dock -> Tron msg -> Tron msg
dock to gui =
    { gui
    | dock = to
    }


{-| Set custom shape for all the GUI, in cells. By default, it is calulated from current viewport size, but you may want to reduce the amount of cells, so here's the method. This way GUI will be perfectly in the middle when docked to central positions.
-}
reshape : ( Int, Int ) -> Tron msg -> Tron msg
reshape cellSize gui =
    { gui
    | size = Just <| Size cellSize
    }


getRootPath : Tron msg -> Path
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



getSizeInCells : Tron msg -> Size Cells
getSizeInCells gui =
    case gui.size of
        Just userSize -> userSize
        Nothing ->
            gui.viewport
                |> sizeFromViewport gui.tree


layout : Tron msg -> ( Property msg, Layout )
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


{-| Subscribe the updates of the GUI, so it would resize with the window,
track mouse etc.

`Sub.map` it to the message that will wrap `Tron.Message` and send it to `update`:

    subscriptions ( myModel, gui ) =
        Sub.batch
            [ ...
            , Tron.subscribe gui |> Sub.map ToTron
            ]
-}
subscriptions : Tron msg -> Sub Msg
subscriptions gui =
    Sub.batch
        [ trackMouse
        -- , Detach.receive gui.detach -- FIXME: use in `WithTron`
        , Browser.onResize <| \w h -> ViewportChanged ( w, h )
        ]


{-| Build the corresponding structure for Tron GUI.

Use it in your `view` function, just `Html.map` it to the message
that will wrap `Tron.Message` and send it to `update`:

    view ( myModel, gui ) =
        Html.div [ ]
            [ gui
                |> Tron.view Tron.Light
                |> Html.map ToTron
            , MyApp.view myModel
            ]

Use `Theme` from `Tron.Style` to set it to `Dark` or `Light` theme.
-}
view : Theme -> Tron msg -> Html Msg
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
