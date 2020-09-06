module Gui.Over exposing (..)


import Array exposing (Array)

import Task

import Gui.Control exposing (..)
import Gui.Control as Control exposing (..)


type alias Label = String

type Icon = Icon String

type alias Shape = ( Int, Int )


type alias Axis =
    { min : Float
    , max : Float
    , step : Float
    , roundBy : Int
    , default : Float
    }


type ExpandState
    = Expanded
    | Collapsed


type ToggleState
    = TurnedOn
    | TurnedOff


type Path = Path (List Int)


type Focus = Focus Int


type alias GroupControl msg =
    Control
        { shape : Shape
        , items : Array ( Label, Over msg )
        }
        { expanded : ExpandState
        , focus : Maybe Focus
        }
        msg


type Over msg
    = Anything
    | Number (Control Axis Float msg)
    | Coordinate (Control ( Axis, Axis ) ( Float, Float ) msg)
    | Text (Control () String msg)
    | Toggle (Control () ToggleState msg)
    | Action (Control (Maybe Icon) () msg)
    | Group (GroupControl msg)


cellWidth = 70
cellHeight = 70
cellMargin = 5


knobDistance = cellHeight * 4


labelColor = "white"
baseColor = "aqua"
onColor = "green"
offColor = "red"
nothingColor = "darkgray"
lineWidth = "2"



-- Recursively try to find the control in the tree, following the given path.
-- When found and the path is valid, respond with the inner control.
-- When the path is invalid (no controls located following these indices), return `Nothing`.
find : Path -> Over msg -> Maybe (Over msg)
find (Path path) root =
    case path of
        [] -> Just root
        index::pathTail ->
            case root of
                Group (Control { items } _ _) ->
                    items
                        |> Array.get index
                        |> Maybe.map Tuple.second
                        |> Maybe.andThen (find <| Path pathTail)
                _ -> Nothing



map : (msgA -> msgB) -> Over msgA -> Over msgB
map f over =
    case over of
        Anything -> Anything
        Number control -> Number <| Control.map f control
        Coordinate control -> Coordinate <| Control.map f control
        Text control -> Text <| Control.map f control
        Toggle control -> Toggle <| Control.map f control
        Action control -> Action <| Control.map f control
        Group (Control setup state handler) -> Group <|
            Control
                { shape = setup.shape
                , items = setup.items
                    |> Array.map (Tuple.mapSecond <| map f)
                }
                state
                (f << handler)


-- fold : TODO


mapReplace : (Path -> Over msg -> Over msg) -> Over msg -> Over msg
mapReplace f root =
    let
        helper (Path curPath) item =
            case item of
                Group (Control config current handler) ->
                    Group
                        (Control
                            { config
                            | items =

                                config.items |>
                                    Array.indexedMap
                                    (\index ( label, innerItem ) ->
                                        ( label
                                        , helper (Path <| curPath ++ [ index ]) innerItem
                                        )
                                    )

                            }
                            current
                            handler
                        )
                _ -> f (Path curPath) item
    in
        helper (Path []) root


updateAt : Path -> (Over msg -> Over msg) -> Over msg -> Over msg
updateAt (Path path) f =
    mapReplace
        <| \(Path otherPath) item ->
            if otherPath == path then f item else item


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
execute : Over msg -> ( Over msg, Cmd msg )
execute item =
    case item of
        Toggle toggleControl ->
            let
                nextToggle = doToggle toggleControl
            in
                ( Toggle nextToggle
                , call nextToggle
                )
        Action control ->
            ( Action control
            , call control
            )
        Group (Control config current handler) ->
            let
                nextState =
                    case current.expanded of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                nextGroup =
                    Control
                        config
                        { current
                        | expanded = nextState
                        }
                        handler
            in
                ( Group nextGroup
                , call nextGroup
                )
        _ -> ( item, Cmd.none )


executeAt : Path -> Over msg -> ( Over msg, Cmd msg )
executeAt path root =
    case root
        |> find path
        |> Maybe.map execute of
        Just ( newCell, cmd ) ->
            ( root |> updateAt path (always newCell)
            , cmd
            )
        Nothing ->
            ( root
            , Cmd.none
            )


doToggle : Control state ToggleState msg -> Control state ToggleState msg
doToggle =
    update
        <| \current ->
            case current of
                TurnedOff -> TurnedOn
                TurnedOn -> TurnedOff


-- updateAndExecute : (v -> v) -> Control s v msg -> ( Control s v msg, msg )
