module Tron.Property exposing (..)


import Array exposing (Array)

import Task

import Color exposing (Color)

import Tron.Path exposing (Path)
import Tron.Path as Path
import Tron.Control as Control exposing (..)
import Tron.Control as Core exposing (Control)

import Tron.Control.Button as Button exposing (..)
import Tron.Control.Number as Number exposing (..)
import Tron.Control.XY as XY exposing (..)
import Tron.Control.Text as Text exposing (..)
import Tron.Control.Color as Color exposing (..)
import Tron.Control.Toggle as Toggle exposing (..)
import Tron.Control.Nest as Nest exposing (..)

import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.PanelShape as Shape exposing (PanelShape)


type FocusAt = FocusAt Int


type alias Shape = ( Float, Float )


type alias NestShape = ( Shape, CellShape )


type alias Label = String


type alias LabelPath = List String


type Property msg
    = Nil
    | Number (Number.Control msg)
    | Coordinate (XY.Control msg)
    | Text (Text.Control msg)
    | Color (Color.Control msg)
    | Toggle (Toggle.Control msg)
    | Action (Button.Control msg)
    | Choice (Maybe FocusAt) NestShape (Nest.ChoiceControl ( Label, Property msg ) msg)
    | Group (Maybe FocusAt) NestShape (Nest.GroupControl ( Label, Property msg ) msg)


type alias ExpandData = List Path


type alias EditingData = List Path


type alias TransientState = ( ExpandData, EditingData )


knobDistance = 90 * 4



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
                        Choice _ _ control ->
                            control
                                |> Nest.get index
                                |> Maybe.andThen (helper pathTail)
                        Group _ _ control ->
                            control
                                |> Nest.get index
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
                        Choice _ _ control ->
                            control
                                |> Nest.get index
                                |> Maybe.map (helper pathTail)
                                |> Maybe.withDefault []
                        Group _ _ control ->
                            control
                                |> Nest.get index
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
        Choice focus shape control ->
            Choice
                focus
                shape
                <| (control
                    |> Nest.mapItems (Tuple.mapSecond <| map f)
                    |> Control.map f)
        Group focus shape control ->
            Group
                focus
                shape
                <| (control
                    |> Nest.mapItems (Tuple.mapSecond <| map f)
                    |> Control.map f)


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
                Choice _ _ control ->
                    f curPath item
                        <| foldItems curPath (Nest.getItems control) val
                Group _ _ control ->
                    f curPath item
                        <| foldItems curPath (Nest.getItems control) val
                _ -> f curPath item val

    in
        helper Path.start root from


unfold : Property msg -> List (Path, Property msg)
unfold =
    fold (\path prop prev -> ( path, prop ) :: prev ) []


-- `replace` -- find better name
replace : (Path -> Property msg -> Property msg) -> Property msg -> Property msg
replace = replaceMap <| always identity


replaceWithLabeledPath
    : (LabelPath -> Property msg -> Property msg) -> Property msg -> Property msg
replaceWithLabeledPath =
    replaceWithLabeledPathMap <| always identity


-- aren't `...Map` functions are compositions like `replace << map`?
replaceMap
    :  (Path -> msgA -> msgB)
    -> (Path -> Property msgB -> Property msgB)
    -> Property msgA
    -> Property msgB
replaceMap msgMap f =
    replaceWithPathsMap (Tuple.first >> msgMap) (Tuple.first >> f)


replaceWithLabeledPathMap
    :  (LabelPath -> msgA -> msgB)
    -> (LabelPath -> Property msgB -> Property msgB)
    -> Property msgA
    -> Property msgB
replaceWithLabeledPathMap msgMap f =
    replaceWithPathsMap (Tuple.second >> msgMap) (Tuple.second >> f)


replaceWithPathsMap
    :  (( Path, LabelPath ) -> msgA -> msgB)
    -> (( Path, LabelPath ) -> Property msgB -> Property msgB)
    -> Property msgA
    -> Property msgB
replaceWithPathsMap msgMap f root =
    -- FIXME: should be just another `fold` actually?

    let

        replaceItem
            :  ( Path, LabelPath )
            -> Int
            -> ( Label, Property msgA )
            -> ( Label, Property msgB )
        replaceItem ( parentPath, parentLabelPath ) index ( label, innerItem ) =
            ( label
            , helper
                ( parentPath |> Path.advance index
                , parentLabelPath ++ [ label ]
                )
                innerItem
            )

        helper : ( Path, LabelPath ) -> Property msgA -> Property msgB
        helper curPath item =
            case item of
                Choice focus shape control ->
                    f curPath
                        <| Choice
                            focus
                            shape
                        <| (control
                                |> Nest.indexedMapItems (replaceItem curPath)
                                |> Control.map (msgMap curPath))
                Group focus shape control ->
                    f curPath
                        <| Group
                            focus
                            shape
                        <| (control
                                |> Nest.indexedMapItems (replaceItem curPath)
                                |> Control.map (msgMap curPath))
                _ -> f curPath <| map (msgMap curPath) <| item

    in
        helper ( Path.start, [] )  root


addPathFrom : Path -> Property msg -> Property ( Path, msg )
addPathFrom from root =
    let
        replaceItem : Path -> Int -> ( Label, Property msg ) -> ( Label, Property (Path, msg) )
        replaceItem parentPath index ( label, innerItem ) =
            ( label
            , helper (parentPath |> Path.advance index) innerItem
            )

        helper : Path -> Property msg -> Property ( Path, msg )
        helper curPath item =
            case item of

                Choice focus shape control ->
                    Choice
                        focus
                        shape
                        (control
                            |> Nest.indexedMapItems (replaceItem curPath)
                            |> Control.map (Tuple.pair curPath)
                        )

                Group focus shape control ->
                    Group
                        focus
                        shape
                        (control
                            |> Nest.indexedMapItems (replaceItem curPath)
                            |> Control.map (Tuple.pair curPath)
                        )

                prop -> map (Tuple.pair curPath) prop

    in
        helper from root


addPath : Property msg -> Property ( Path, msg )
addPath =
    replaceMap (Tuple.pair) (always identity)


addLabeledPath : Property msg -> Property ( LabelPath, msg )
addLabeledPath =
    replaceWithLabeledPathMap (Tuple.pair) (always identity)


addPaths : Property msg -> Property ( ( Path, LabelPath ), msg )
addPaths =
    replaceWithPathsMap (Tuple.pair) (always identity)


updateAt : Path -> (Property msg -> Property msg) -> Property msg -> Property msg
updateAt path f =
    replace
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
            Just <| Toggle <| Toggle.toggle toggleControl
        Action control ->
            -- we don't update the value since it's `()`, but we do execute it
            Just <| Action control
        Text textControl ->
            Just <| Text <| Text.ensureEditing textControl
        Choice focus shape control ->
            Just
                <| Choice focus shape
                <| Nest.toggle control
        Group focus shape control ->
            Just
                <| Group focus shape
                <| Nest.toggle control
        _ -> Nothing


executeAt : Path -> Property msg -> List ( Path, Property msg )
executeAt path root =
    case root
        |> findWithParent path of
        Just ( parent, item ) ->
            case ( parent, item ) of
                ( Choice focus shape control, Action _ ) ->

                    case Path.pop path of
                        Just ( toParent, selectedIndex ) ->
                            let
                                newParent =
                                    select selectedIndex control
                            in
                                case execute item of
                                    Just newCell ->
                                        [ ( toParent, Choice focus shape newParent )
                                        , ( path, newCell )
                                        ]
                                    Nothing ->
                                        [ (toParent, Choice focus shape newParent )
                                        ]
                        Nothing ->
                            []

                ( _, _ ) ->

                    case execute item of
                        Just newCell -> [ ( path, newCell ) ]
                        Nothing -> []

        Nothing -> []



-- TODO: make `Control` decide what transient state is:
-- it is smth that should be kept when the value is replaced,
-- but structure is not affected (i.e. `Form` for the nests)
loadTransientState : Property msg -> TransientState
loadTransientState prop =
    ( loadExpanded prop
    , loadTextEditing prop
    )


loadExpanded : Property msg -> ExpandData
loadExpanded =
    unfold
        >> List.filterMap
            (\(path, innerProp) ->
                case innerProp of
                    Group _ _ control ->
                        if control |> Nest.is Expanded then Just path else Nothing
                    Choice _ _ control ->
                        if control |> Nest.is Expanded then Just path else Nothing
                    _ -> Nothing
            )


loadTextEditing : Property msg -> EditingData
loadTextEditing =
    unfold
        >> List.filterMap
            (\(path, innerProp) ->
                case innerProp of
                    Text (Control _ ( Text.Editing, _) _ ) ->
                        Just path
                    _ -> Nothing
            )


applyExpanded : Property msg -> ExpandData -> Property msg
applyExpanded =
    -- expanded
    --     |> List.foldl expandAt prop
    List.foldl expandAt -- FIXME: not very efficient since goes through the structure several times


applyTextEditing : Property msg -> EditingData -> Property msg
applyTextEditing =
    -- expanded
    --     |> List.foldl expandAt prop
    List.foldl ensureEditingAt -- FIXME: not very efficient since goes through the structure several times


-- FIXME: instead, traverse two trees with the same structure and move transient states between them
applyTransientState : Property msg -> TransientState -> Property msg
applyTransientState prop ( expanded, editing ) =
    editing |> applyTextEditing (expanded |> applyExpanded prop)


-- TODO: better use the functions below directly from their controls


finishEditingAt : Path -> Property msg -> Property msg
finishEditingAt path =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text <| Text.finishEditing control
                _ -> prop


updateTextAt : Path -> String -> Property msg -> Property msg
updateTextAt path newValue =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text ( control |> Text.updateText newValue )
                _ -> prop



-- updateAndExecute : (v -> v) -> Control s v msg -> ( Control s v msg, msg )

ensureEditing : Property msg -> Property msg
ensureEditing prop =
    case prop of
        Text control ->
            Text <| Text.ensureEditing control
        _ -> prop


expand : Property msg -> Property msg
expand prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.expand control
        Choice focus shape control ->
            Choice focus shape <| Nest.expand control
        _ -> prop


collapse : Property msg -> Property msg
collapse prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.collapse control
        Choice focus shape control ->
            Choice focus shape <| Nest.collapse control
        _ -> prop


detach : Property msg -> Property msg
detach prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.detach control
        Choice focus shape control ->
            Choice focus shape <| Nest.detach control
        _ -> prop


expandAt : Path -> Property msg -> Property msg
expandAt path =
    updateAt path expand


detachAt : Path -> Property msg -> Property msg
detachAt path =
    updateAt path detach


detachAll : Property msg -> Property msg
detachAll =
    replace <| always detach


toggle : Property msg -> Property msg
toggle prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggle control
        _ -> prop


toggleAt : Path -> Property msg -> Property msg
toggleAt path =
    updateAt path toggle


toggleOn : Property msg -> Property msg
toggleOn prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggleOn control
        _ -> prop


toggleOff : Property msg -> Property msg
toggleOff prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggleOff control
        _ -> prop


ensureEditingAt : Path -> Property msg -> Property msg
ensureEditingAt path =
    updateAt path ensureEditing


{-
reshape : Shape -> Property msg -> Property msg
reshape shape prop =
    case prop of
        Group ( Control ( _, items ) ( expanded, focus ) handler ) ->
            Group ( Control ( shape, items ) ( expanded, focus ) handler )
        _ -> prop
-}


isGhost : Property msg -> Bool
isGhost prop =
    case prop of
        Nil -> True
        _ -> False


noGhosts : List (Property msg) -> List (Property msg)
noGhosts = List.filter (not << isGhost)


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
        Choice _ _ control -> Control.call control
        Group _ _ control -> Control.call control


getCellShape : Property msg -> Maybe CellShape
getCellShape prop =
    case prop of
        Choice _ ( _, cellShape ) _ ->
            Just cellShape
        Group _ ( _, cellShape ) _ ->
            Just cellShape
        _ -> Nothing


getSelected : Property msg -> Maybe ( Label, Property msg )
getSelected prop =
    case prop of
        Choice _ _ control ->
            control |> Nest.getSelected
        _ -> Nothing


isSelected : Property msg -> Int -> Bool
isSelected prop index =
    case prop of
        Choice _ _ control ->
            Nest.isSelected control index
        _ -> False


findShape : CellShape -> PanelShape -> List (Property msg) -> ( Float, Float )
findShape cellShape panelShape =
    noGhosts
        >> Shape.find cellShape panelShape
