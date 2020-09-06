module Gui.Over exposing (..)


import Array exposing (Array)

import Task

import Gui.Control exposing (..)
import Gui.Control as Control exposing (..)


type alias Color = String


type alias Label = String

type Icon = Icon String

type alias Shape = ( Int, Int )


type alias Axis =
    { min : Float
    , max : Float
    , step : Float
    -- , roundBy : Int
    -- , default : Float
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
    = Nil
    | Number (Control Axis Float msg)
    | Coordinate (Control ( Axis, Axis ) ( Float, Float ) msg)
    | Text (Control () String msg)
    | Color (Control () Color msg)
    | Toggle (Control () ToggleState msg)
    | Action (Control ( Maybe Icon ) () msg)
    | Choice (Control ( Array ( Maybe Icon, Label ) ) Int msg)
    | Group
        (Control
            ( Shape, Array ( Label, Over msg ) )
            ( ExpandState, Maybe Focus )
            msg
        )


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
                Group (Control ( _, items ) _ _) ->
                    items
                        |> Array.get index
                        |> Maybe.map Tuple.second
                        |> Maybe.andThen (find <| Path pathTail)
                _ -> Nothing



map : (msgA -> msgB) -> Over msgA -> Over msgB
map f over =
    case over of
        Nil -> Nil
        Number control -> Number <| Control.map f control
        Coordinate control -> Coordinate <| Control.map f control
        Text control -> Text <| Control.map f control
        Color control -> Color <| Control.map f control
        Toggle control -> Toggle <| Control.map f control
        Action control -> Action <| Control.map f control
        Choice control -> Choice <| Control.map f control
        Group (Control ( shape, items ) state handler) ->
            Group <|
                Control
                    ( shape
                    , items
                        |> Array.map
                            (Tuple.mapSecond <| map f)
                    )
                    state
                    (f << handler)


-- fold : TODO


mapReplace : (Path -> Over msg -> Over msg) -> Over msg -> Over msg
mapReplace f root =
    let
        helper (Path curPath) item =
            case item of
                Group (Control ( shape, items ) state handler) ->
                    Group
                        (Control
                            ( shape
                            , items |>
                                Array.indexedMap
                                (\index ( label, innerItem ) ->
                                    ( label
                                    , helper (Path <| curPath ++ [ index ]) innerItem
                                    )
                                )
                            )
                            state
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
        Group (Control setup ( expanded, focus ) handler) ->
            let
                nextState =
                    case expanded of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                nextGroup =
                    Control
                        setup
                        ( nextState
                        , focus
                        )
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


toggleToBool : ToggleState -> Bool
toggleToBool state =
    case state of
        TurnedOn -> True
        TurnedOff -> False


boolToToggle : Bool -> ToggleState
boolToToggle bool =
    case bool of
        True -> TurnedOn
        False -> TurnedOff
