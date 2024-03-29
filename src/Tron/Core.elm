module Tron.Core exposing
    ( State, Msg, Error
    , view, update, init, subscriptions, run
    , dock, reshape
    , applyRaw, tryDeduce
    )


import Browser.Dom as Dom
import Browser.Events as Browser
import Task
import Html exposing (Html)
import Html.Events as HE
import Json.Encode as E
import Json.Decode as D

import Size exposing (..)

import Tron exposing (Tron)
import Tron as Tron
import Tron.Path exposing (Path)
import Tron.Path as Path
import Tron.Control exposing (..)
import Tron.Tree.Internals as Tree exposing (Tree(..))
import Tron.Tree.Paths as Tree
import Tron.Tree.Events as Tree
import Tron.Tree.Controls as Tree
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
import Tron.Control.Impl.Nest as Nest
import Tron.Build exposing (..)
import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Tree as Exp
import Tron.Tree.Expose.Convert as Exp

import Tron.Style.Dock exposing (Dock(..))
import Tron.Style.Dock as Dock exposing (..)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Logic as Style exposing (..)
import Tron.Style.Logic as Dock
import Tron.Style.Cell as Cell exposing (..)

import Tron.Control.Value as Value
import Tron.Control.Action as A


type alias State =
    { dock : Dock
    , viewport : Size Pixels
    , size : Maybe (Size Cells)
    , mouse : MouseState
    , detach : ( Maybe ClientId, Detach.State )
    , hidden : Bool
    , errors : List Error
    }


type alias Error = String


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
        , errors = []
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


-- overMap : (msgA -> msgB) -> Tree msgA -> Model msgB -> Model msgB
-- overMap f prop =
--     over <| Tron.Tree.map f prop


update
    :  Msg
    -> State
    -> Tron msg
    -> ( State, Tree (), Cmd (Exp.Out, msg) )
update msg state tree  =
    case msg of
        NoOp ->
            ( state, tree |> Tree.toUnit, Cmd.none )

        ApplyMouse mouseAction ->
            let
                ( nextState, changesTree ) =
                    handleMouse mouseAction state (tree |> Tree.toUnit)
            in
                ( nextState
                , changesTree |> Tree.toUnit
                , fireChangesFrom nextState ( tree, changesTree )
                )

        Click path ->
            let
                changesTree =
                    tree |> Tree.setAll A.Stay
                updates =
                    changesTree
                        |> Tree.toUnit
                        |> Tree.executeAt path
                        |> List.map (Tuple.mapSecond <| Tree.set A.Fire)
                nextRoot =
                    changesTree
                        |> Tree.updateMany updates

            in
                ( state
                , nextRoot |> Tree.toUnit
                , fireChangesFrom state ( tree, nextRoot )
                )

        MouseDown path ->
            ( state
            , Focus.on tree path |> Tree.toUnit
            , Cmd.none
            )

        KeyDown keyCode ->
           let
                curFocus = Focus.find tree
                ( nextState, changesTree ) =
                    handleKeyDown keyCode curFocus state (tree |> Tree.toUnit)
            in
                ( nextState
                , changesTree |> Tree.toUnit
                , fireChangesFrom nextState ( tree, changesTree )
                )

        ViewportChanged ( w, h ) ->
            (
                { state
                | viewport = Size (w, h)
                }
            , tree |> Tree.toUnit
            , Cmd.none
            )

        TextInput path val -> -- FIXME: To control `Action`
            let
                nextRoot =
                    tree
                        |> Tree.updateTextAt path val
            in
                ( state
                , nextRoot |> Tree.toUnit
                , Cmd.none
                )

        Detach path ->
            let
                nextRoot = Tree.detachAt path tree
            in
                ( state
                , nextRoot |> Tree.toUnit
                , Cmd.none -- FIXME: Detach.sendTree gui.detach nextRoot
                )

        SwitchPage path pageNum ->
            let
                nextRoot = Tree.switchPageAt path pageNum tree
            in
                ( state
                , nextRoot |> Tree.toUnit
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
    case tryDeduce (tree |> Tree.toUnit) toDeduce of
        Just rawUpdate -> tree |> applyRaw rawUpdate
        Nothing -> Cmd.none


applyHandlers : Tron msg -> Tree (Maybe msg)
applyHandlers = Tree.apply


tryDeduce : Tree () -> Exp.DeduceIn -> Maybe Exp.In
tryDeduce tree { path, value } =
    tree
        |> Tree.find (Path.fromList path)
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
                , type_ = prop |> Tree.getValue |> Value.getTypeString
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



handleMouse : MouseAction -> State -> Tree () -> ( State, Tree A.Change )
handleMouse mouseAction state tree =
    let
        --rootPath = getRootPath state
        changesTree =
            tree |> Tree.setAll A.Stay
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

        keepsDragging = curMouseState.down

        startedDragging =
            case mouseAction of
                Mouse.Down _ ->
                    not curMouseState.down
                    && nextMouseState.down
                _ -> False

        finishedDragging =
            (case mouseAction of
                Mouse.Up _ ->
                    curMouseState.down
                    && not nextMouseState.down
                _ -> False
            )

        ( dX, dY ) = distanceXY knobDistance nextMouseState

        mouseActions
            = case ( startedDragging, keepsDragging, finishedDragging ) of
                ( True, True, _ ) ->
                    [ A.DragStart, A.Dragging { dx = dX, dy = dY } ]
                    -- [ A.DragStart ] -- FIXME: it seems it is ok to fold just to one action here
                ( True, _, _ ) ->
                    [ A.DragStart ]
                ( False, True, False ) ->
                    [ A.Dragging { dx = dX, dy = dY } ]
                ( False, True, True ) ->
                    [ A.DragFinish ]
                ( _, _, True ) ->
                    [ A.DragFinish ]
                _ ->
                    []

        maybePathAtCursor =
            if finishedDragging  then
                curMouseState.dragFrom |> Maybe.andThen findPathAt
            else if startedDragging || keepsDragging then
                nextMouseState.dragFrom |> Maybe.andThen findPathAt
            else Nothing

        applyActions property =
            List.foldl
                (\action -> Tree.toUnit >> Tree.update action)
                property
                mouseActions
            |> forceIfLive

        forceIfLive property = -- FIXME: should be forces in the `update` code for any `Live` property, not only regading the mouse events
            case property of
                Live liveProp -> liveProp |> Tree.set A.Fire |> Live
                _ -> property

        nextTree =
            case maybePathAtCursor of
                Just path ->
                    Tree.updateAt
                        path
                        applyActions
                        changesTree
                Nothing -> changesTree

    in

        (
            { state
            | mouse = nextMouseState
            }
        ,
            if startedDragging then
                maybePathAtCursor
                    |> Maybe.map (Focus.on nextTree)
                    |> Maybe.withDefault nextTree
            else nextTree

        )


handleKeyDown
    :  Int
    -> Path
    -> State
    -> Tree ()
    -> ( State, Tree A.Change )
handleKeyDown keyCode path state tree =
    let

        changesTree =
            tree |> Tree.setAll A.Stay

        executeByPath _ = -- uses only `gui.tree`
            let
                updates = changesTree |> Tree.toUnit |> Tree.executeAt path
                markedUpdates =
                    updates |> List.map (Tuple.mapSecond <| Tree.set A.Fire)
                nextRoot = changesTree |> Tree.updateMany markedUpdates
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
            case changesTree |> Tree.find path of
                Just prop ->
                    let
                        nextProp =
                            prop |> Tree.toUnit |> Tree.update A.Exit -- FIXME: exit?? not enter??
                    in
                        ( state
                        ,
                            -- FIXME: second time search for a path
                            changesTree
                                |> Tree.replaceAt path nextProp
                        -- FIXME: inside, we check if it is a text prop again
                        -- , Tron.perform nextProp
                            --|> Cmd.map (Tuple.pair path)
                        )
                _ -> executeByPath ()
        -- else
        _ -> ( state, changesTree )


expose : State -> Tree a -> Tree Exp.Out
expose state =
    Exp.expose
        >> Tree.map
            ( (\val ->
                { update = val
                , client =  Detach.encodeClientId <| Tuple.first <| state.detach
                }
            ) )


expose_ : State -> Tree a -> Tree ( Exp.Out, a )
expose_ state tree =
    tree
        |> Tree.squeezeMap2 Tuple.pair (tree |> expose state)


collectChanges : State -> Tree ( A.Change, Maybe msg ) -> List (Exp.Out, msg)
collectChanges state =
    expose_ state
        >> Tree.unfold
        >> List.map (Tuple.second >> Tree.get)
        >> List.filterMap
            (\(expOut, (valState, maybeMsg)) ->
                case ( valState, maybeMsg ) of
                    ( A.Fire, Just msg ) ->
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


fireChangesFrom : State -> ( Tron msg, Tree A.Change ) -> Cmd (Exp.Out, msg)
fireChangesFrom state ( tron, changesTree ) =
    Tree.squeezeMap3
        (\handler currentVal valueState ->
            ( valueState, handler currentVal )
        )
        tron
        (changesTree |> Tree.proxify)
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


sizeFromViewport : Tree a -> Size Pixels -> Size Cells
sizeFromViewport _ (Size ( widthInPixels, heightInPixels )) =
    {- let
        cellsFitHorizontally = floor (toFloat widthInPixels / Cell.width)
        cellsFitVertically = floor (toFloat heightInPixels / Cell.height)
    in -}
        ( floor <| toFloat widthInPixels / Cell.width
        , floor <| toFloat heightInPixels / Cell.height
        ) |> Size



getSizeInCells : State -> Tree a -> Size Cells
getSizeInCells state tree =
    case state.size of
        Just userSize -> userSize
        Nothing ->
            state.viewport
                |> sizeFromViewport tree


layout : State -> Tree a -> ( Tree a, Layout )
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
                    |> Tree.find path
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


view : Mode -> Theme -> State -> Tree a -> Html Msg
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
