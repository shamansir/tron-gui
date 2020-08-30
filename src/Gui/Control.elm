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


type alias GroupControl msg =
    Control
        { shape : Shape
        , items : Array ( Label, Over msg )
        }
        { expanded : ExpandState
        , focus :
            Maybe
                { index : Int
                , position : ( Int, Int )
                }
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
follow : Path -> Over msg -> Maybe (Over msg)
follow (Path path) root =
    case path of
        [] -> Just root
        index::pathTail ->
            case root of
                Group (Control { items } _ _) ->
                    items
                        |> Array.get index
                        |> Maybe.map Tuple.second
                        |> Maybe.andThen (follow <| Path pathTail)
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


-- for mouse click or enter key handling
execute : Over msg -> ( Over msg, Cmd msg )
execute item =
    case item of
        Toggle (Control _ current handler) ->
            let
                nextState =
                    case current of
                        TurnedOff -> TurnedOn
                        TurnedOn -> TurnedOff
                nextToggle = Control () nextState handler
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


executeAt : Path -> Over msg -> ( Over msg, Cmd msg )
executeAt path root =
    case root
        |> follow path
        |> Maybe.map execute of
        Just ( newCell, cmd ) ->
            ( root |> updateAt path (always newCell)
            , cmd
            )
        Nothing ->
            ( root
            , Cmd.none
            )


-- call control handler with its current value
callHandler : Control s v msg -> Cmd msg
callHandler (Control _ current handler) =
    Task.succeed current
        |> Task.perform handler


-- updateAndExecute : (v -> v) -> Control s v msg -> ( Control s v msg, msg )
