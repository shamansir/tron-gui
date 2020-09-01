module Gui.Control exposing (..)


import Array exposing (Array)

import Task


type Control setup value msg =
    Control setup value (value -> msg)


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


type AlterKnob
    = Stay
    | Alter Float -- from -0.5 to 0.5


type AlterXY
    = Stay_
    | Alter_ ( Float, Float ) -- both from -0.5 to 0.5


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



-- map : TODO


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



doToggle : Control state ToggleState msg -> Control state ToggleState msg
doToggle =
    update
        <| \current ->
            case current of
                TurnedOff -> TurnedOn
                TurnedOn -> TurnedOff


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
                , callHandler nextToggle
                )
        Action control ->
            ( Action control
            , callHandler control
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
                , callHandler nextGroup
                )
        _ -> ( item, Cmd.none )


focusOn : Path -> Over msg -> Over msg
focusOn (Path path) root =
    case ( path, root ) of
        ( [], _ ) -> root
        ( x::xs, Group (Control config current handler) ) ->
            Group
                (Control
                    { config
                    | items =

                        config.items |>
                                    Array.indexedMap
                                    (\index ( label, innerItem ) ->
                                        ( label
                                        , if index == x
                                            then focusOn (Path xs) innerItem
                                            else innerItem

                                        )
                                    )

                    }
                    current
                    handler
                )
        ( _, _ ) -> root



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


update : (v -> v) -> Control s v msg -> Control s v msg
update f (Control state current handler) =
    Control state (f current) handler


-- call control handler with its current value
callHandler : Control s v msg -> Cmd msg
callHandler (Control _ current handler) =
    Task.succeed current
        |> Task.perform handler


-- updateAndExecute : (v -> v) -> Control s v msg -> ( Control s v msg, msg )
