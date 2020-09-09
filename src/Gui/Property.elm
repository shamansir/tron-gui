module Gui.Property exposing (..)


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
        ( Shape, Array ( Label, Property msg ) )
        ( ExpandState, Maybe Focus )
        msg


type alias ChoiceControl msg =
    Control
        ( Shape, Array ( Label, Property msg ) )
        ( ExpandState, Int )
        msg



type Property msg
    = Nil
    | Number (Control Axis Float msg)
    | Coordinate (Control ( Axis, Axis ) ( Float, Float ) msg)
    | Text (Control () String msg)
    | Color (Control () Color msg)
    | Toggle (Control () ToggleState msg)
    | Action (Control ( Maybe Icon ) () msg)
    | Choice (ChoiceControl msg)
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
find : Path -> Property msg -> Maybe (Property msg)
find path =
    find1 path
        >> Maybe.map Tuple.second


find1 : Path -> Property msg -> Maybe (Label, Property msg)
find1 (Path path) root =
    let
        helper ipath ( label, prop ) =
            case ipath of
                [] -> Just ( label, prop )
                index::pathTail ->
                    case prop of
                        Choice (Control ( _, items ) _ _) ->
                            items
                                |> Array.get index
                                |> Maybe.andThen (helper pathTail)
                        Group (Control ( _, items ) _ _) ->
                            items
                                |> Array.get index
                                |> Maybe.andThen (helper pathTail)
                        _ -> Nothing
    in
        helper path ( "", root )



map : (msgA -> msgB) -> Property msgA -> Property msgB
map f prop =
    case prop of
        Nil -> Nil
        Number control -> Number <| Control.map f control
        Coordinate control -> Coordinate <| Control.map f control
        Text control -> Text <| Control.map f control
        Color control -> Color <| Control.map f control
        Toggle control -> Toggle <| Control.map f control
        Action control -> Action <| Control.map f control
        Choice (Control ( shape, items ) state handler) ->
            Choice <|
                Control
                    ( shape
                    , items
                        |> Array.map
                            (Tuple.mapSecond <| map f)
                    )
                    state
                    (f << handler)
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


fold : (Path -> Property msg -> a -> a) -> a -> Property msg -> a
fold f from root =
    let

        foldItems : Path -> Array ( Label, Property msg ) -> a -> a
        foldItems (Path curPath) items val =
            items
                |> Array.map Tuple.second
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, innerItem) prev ->
                        helper (Path <| curPath ++ [ index ]) innerItem prev
                    )
                    val

        helper : Path -> Property msg -> a -> a
        helper curPath item val =
            case item of
                Choice (Control ( _, items ) _ _) ->
                    f curPath item
                        <| foldItems curPath items val
                Group (Control ( _, items ) _ _) ->
                    f curPath item
                        <| foldItems curPath items val
                _ -> f curPath item val

    in
        helper (Path []) root from


mapReplace : (Path -> Property msg -> Property msg) -> Property msg -> Property msg
mapReplace f root =
    let

        replaceItems : Path -> Array ( Label, Property msg ) -> Array ( Label, Property msg )
        replaceItems (Path curPath) items =
            items |>
                Array.indexedMap
                (\index ( label, innerItem ) ->
                    ( label
                    , helper (Path <| curPath ++ [ index ]) innerItem
                    )
                )

        helper : Path -> Property msg -> Property msg
        helper curPath item =
            case item of
                Choice (Control ( shape, items ) state handler) ->
                    f curPath
                        <| Choice
                            (Control
                                ( shape
                                , replaceItems curPath items
                                )
                                state
                                handler
                            )
                Group (Control ( shape, items ) state handler) ->
                    f curPath
                        <| Group
                            (Control
                                ( shape
                                , replaceItems curPath items
                                )
                                state
                                handler
                            )
                _ -> f curPath item

    in
        helper (Path []) root


updateAt : Path -> (Property msg -> Property msg) -> Property msg -> Property msg
updateAt (Path path) f =
    mapReplace
        <| \(Path otherPath) item ->
            if otherPath == path then f item else item


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
execute : Property msg -> ( Property msg, Cmd msg )
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
        Choice (Control setup ( expanded, selected ) handler) ->
            let
                nextState =
                    case expanded of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                nextChoice =
                    Control
                        setup
                        ( nextState
                        , selected
                        )
                        handler
            in
                ( Choice nextChoice
                , call nextChoice
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


executeAt : Path -> Property msg -> ( Property msg, Cmd msg )
executeAt path root =
    case root
        |> find path
        |> Maybe.map execute of
        Just ( newCell, cmd ) ->
            -- TODO: we search for it second time here, fix it
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
