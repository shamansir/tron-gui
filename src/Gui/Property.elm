module Gui.Property exposing (..)


import Array exposing (Array)

import Task

import Color exposing (Color)

import Gui.Path exposing (Path)
import Gui.Path as Path
import Gui.Control exposing (..)
import Gui.Control as Control exposing (..)


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


type GroupState
    = Expanded
    | Collapsed
    | Detached


type ToggleState
    = TurnedOn
    | TurnedOff


type FocusAt = FocusAt Int


type Selected = Selected Int


type TextState
    = Ready
    | Editing


type alias GroupControl msg =
    Control
        ( Shape, Array ( Label, Property msg ) )
        ( GroupState, Maybe FocusAt )
        msg


type alias ChoiceControl msg =
    Control
        ( Shape, Array ( Label, Property msg ) ) -- FIXME: Control ( Maybe Icon ) () msg
        ( GroupState, ( Maybe FocusAt, Selected ) )
        msg


type Property msg
    = Nil
    | Number (Control Axis Float msg)
    | Coordinate (Control ( Axis, Axis ) ( Float, Float ) msg)
    | Text (Control () ( TextState, String ) msg)
    | Color (Control () Color msg)
    | Toggle (Control () ToggleState msg)
    | Action (Control ( Maybe Icon ) () msg)
    | Choice (ChoiceControl msg)
    -- | ChoiceItem (Control ( Maybe Icon ) () msg)
    | Group (GroupControl msg)


cellWidth : Float
cellWidth = 73
cellHeight : Float
cellHeight = 73


cellMargin : Float
cellMargin = 5


knobDistance = 90 * 4


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
find1 path root = -- TODO: reuse `fildAll` + `tail`?
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
        helper (Path.toList path) ( "", root )


findWithParent : Path -> Property msg -> Maybe ( Property msg, Property msg )
findWithParent path =
    findWithParent1 path >> Maybe.map (Tuple.mapBoth Tuple.second Tuple.second)


findWithParent1 : Path -> Property msg -> Maybe ( (Label, Property msg), (Label, Property msg) )
findWithParent1 path root =
    let
        allArray = findAll path root |> Array.fromList
    in
        Maybe.map2
            Tuple.pair
            (allArray |> Array.get (Array.length allArray - 2))
            (allArray |> Array.get (Array.length allArray - 1))


findAll : Path -> Property msg -> List (Label, Property msg)
findAll path root =
    let
        helper ipath ( label, prop ) =
            ( label, prop ) :: case ipath of
                [] -> []
                index::pathTail ->
                    case prop of
                        Choice (Control ( _, items ) _ _) ->
                            items
                                |> Array.get index
                                |> Maybe.map (helper pathTail)
                                |> Maybe.withDefault []
                        Group (Control ( _, items ) _ _) ->
                            items
                                |> Array.get index
                                |> Maybe.map (helper pathTail)
                                |> Maybe.withDefault []
                        _ -> [ ]
    in
        helper (Path.toList path) ( "", root )


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
        foldItems curPath items val =
            items
                |> Array.map Tuple.second
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, innerItem) prev ->
                        helper (curPath |> Path.advance index) innerItem prev
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
        helper Path.start root from


unfold : Property msg -> List (Path, Property msg)
unfold =
    fold (\path prop prev -> ( path, prop ) :: prev ) []


mapReplace : (Path -> Property msg -> Property msg) -> Property msg -> Property msg
mapReplace f root =
    let

        replaceItems : Path -> Array ( Label, Property msg ) -> Array ( Label, Property msg )
        replaceItems curPath items =
            items |>
                Array.indexedMap
                (\index ( label, innerItem ) ->
                    ( label
                    , helper (curPath |> Path.advance index) innerItem
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
        helper Path.start root


updateAt : Path -> (Property msg -> Property msg) -> Property msg -> Property msg
updateAt path f =
    mapReplace
        <| \otherPath item ->
            if Path.equal otherPath path then f item else item


updateMany : List ( Path, Property msg ) -> Property msg -> Property msg
updateMany updates root =
    List.foldl
        (\(path, nextProp) lastRoot ->
            lastRoot |> updateAt path (always nextProp)
        )
        root
        updates


setAt : Path -> Property msg -> Property msg -> Property msg
setAt path newProperty =
    updateAt path <| always newProperty


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
-- FIXME: should not call controls itself, only return the update
execute : Property msg -> Maybe (Property msg)
execute item =
    case item of
        Toggle toggleControl ->
            Just <| Toggle <| doToggle toggleControl
        Action control ->
            -- we don't update the value since it's `()`, but we do execute it
            Just <| Action control
        Text textControl ->
            Just <| Text <| ensureEditing textControl
        Choice (Control setup ( expanded, selected ) handler) ->
            let
                nextState =
                    case expanded of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                        Detached -> Detached

            in
                Just
                    <| Choice
                    <| Control
                        setup
                        ( nextState
                        , selected
                        )
                        handler
        Group (Control setup ( expanded, focus ) handler) ->
            let
                nextState =
                    case expanded of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                        Detached -> Detached

            in
                Just
                    <| Group
                    <| Control
                        setup
                        ( nextState
                        , focus
                        )
                        handler
        _ -> Nothing


executeAt : Path -> Property msg -> List ( Path, Property msg )
executeAt path root =
    case root
        |> findWithParent path of
        Just ( parent, item ) ->
            case ( parent, item ) of
                ( Choice control, Action _ ) ->

                    case Path.pop path of
                        Just ( toParent, selectedIndex ) ->
                            let
                                newParent =
                                    select selectedIndex control
                            in
                                case execute item of
                                    Just newCell ->
                                        [ ( toParent, Choice newParent )
                                        , ( path, newCell )
                                        ]
                                    Nothing ->
                                        [ ( toParent, Choice newParent )
                                        ]
                        Nothing ->
                            []

                ( _, _ ) ->

                    case execute item of
                        Just newCell -> [ ( path, newCell ) ]
                        Nothing -> []

        Nothing -> []


doToggle : Control setup ToggleState msg -> Control setup ToggleState msg
doToggle =
    update
        <| \current ->
            case current of
                TurnedOff -> TurnedOn
                TurnedOn -> TurnedOff


ensureEditing : Control setup ( TextState, a ) msg -> Control setup ( TextState, a ) msg
ensureEditing =
    update
        <| \(_, v) -> ( Editing, v )


finishEditing : Control setup ( TextState, a ) msg -> Control setup ( TextState, a ) msg
finishEditing =
    update
        <| \(_, v) -> ( Ready, v )


finishEditingAt : Path -> Property msg -> Property msg
finishEditingAt path =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text <| finishEditing control
                _ -> prop


updateText : String -> Control setup ( a, String ) msg -> Control setup ( a, String ) msg
updateText newValue =
    update
        <| \(s, v) -> ( s, newValue )


updateTextAt : Path -> String -> Property msg -> Property msg
updateTextAt path newValue =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text <| updateText newValue control
                _ -> prop


select : Int -> ChoiceControl msg -> ChoiceControl msg
select index (Control (shape, items) ( expanded, ( focus, _ ) ) handler) =
    Control (shape, items) ( expanded, ( focus, Selected index ) ) handler


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


expand : Property msg -> Property msg
expand prop =
    case prop of
        Group ( Control setup ( _, focus ) handler ) ->
            Group ( Control setup ( Expanded, focus ) handler )
        Choice ( Control setup ( _, selection ) handler ) ->
            Choice ( Control setup ( Expanded, selection ) handler )
        _ -> prop


expandAt : Path -> Property msg -> Property msg
expandAt path =
    updateAt path expand



collapse : Property msg -> Property msg
collapse prop =
    case prop of
        Group ( Control setup ( _, focus ) handler ) ->
            Group ( Control setup ( Collapsed, focus ) handler )
        Choice ( Control setup ( _, selection ) handler ) ->
            Choice ( Control setup ( Collapsed, selection ) handler )
        _ -> prop


detach : Property msg -> Property msg
detach prop =
    case prop of
        Group ( Control setup ( _, focus ) handler ) ->
            Group ( Control setup ( Detached, focus ) handler )
        Choice ( Control setup ( _, selection ) handler ) ->
            Choice ( Control setup ( Detached, selection ) handler )
        _ -> prop


detachAt : Path -> Property msg -> Property msg
detachAt path =
    updateAt path detach


attach : Property msg -> Property msg
attach prop =
    case prop of
        Group ( Control setup ( _, focus ) handler ) ->
            Group ( Control setup ( Expanded, focus ) handler )
        Choice ( Control setup ( _, selection ) handler ) ->
            Choice ( Control setup ( Expanded, selection ) handler )
        _ -> prop


attachAt : Path -> Property msg -> Property msg
attachAt path =
    updateAt path attach


detachAll : Property msg -> Property msg
detachAll =
    mapReplace <| always detach


toggle : Property msg -> Property msg
toggle prop =
    let
        invert current =
            case current of
                TurnedOff -> TurnedOn
                TurnedOn -> TurnedOff
    in
    case prop of
        Toggle ( Control setup current handler ) ->
            Toggle ( Control setup (invert current) handler )
        _ -> prop


toggleAt : Path -> Property msg -> Property msg
toggleAt path =
    updateAt path toggle


toggleOn : Property msg -> Property msg
toggleOn prop =
    case prop of
        Toggle ( Control setup _ handler ) ->
            Toggle ( Control setup TurnedOn handler )
        _ -> prop


toggleOff : Property msg -> Property msg
toggleOff prop =
    case prop of
        Toggle ( Control setup _ handler ) ->
            Toggle ( Control setup TurnedOff handler )
        _ -> prop


reshape : Shape -> Property msg -> Property msg
reshape shape prop =
    case prop of
        Group ( Control ( _, items ) ( expanded, focus ) handler ) ->
            Group ( Control ( shape, items ) ( expanded, focus ) handler )
        _ -> prop


isGhost : Property msg -> Bool
isGhost prop =
    case prop of
        Nil -> True
        _ -> False


noGhosts : Array (Property msg) -> Array (Property msg)
noGhosts = Array.filter (not << isGhost)


call : Property msg -> Cmd msg
call prop =
    case prop of
        Nil -> Cmd.none
        Number control -> Control.call control
        Coordinate control -> Control.call control
        Text control -> Control.call control
        Color control -> Control.call control
        Toggle control -> Control.call control
        Action control -> Control.call control
        Choice control -> Control.call control
        Group control -> Control.call control


withItem
     : Int
    -> (Property msg -> Property msg)
    -> Control ( a, Array ( b, Property msg ) ) value msg
    -> Control ( a, Array ( b, Property msg ) ) value msg
withItem id f ( Control ( shape, items ) state handler ) =
    Control
        ( shape
        , case Array.get id items of
            Just ( label, innerProp ) ->
                items
                |> Array.set id
                    ( label
                    , f innerProp
                    )
            Nothing -> items
        )
        state
        handler
