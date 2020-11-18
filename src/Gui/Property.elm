module Gui.Property exposing (..)


import Array exposing (Array)

import Task

import Color exposing (Color)

import Gui.Path exposing (Path)
import Gui.Path as Path
import Gui.Control as Control exposing (..)
import Gui.Control as Core exposing (Control)

import Gui.Control.Button as Button exposing (..)
import Gui.Control.Number as Number exposing (..)
import Gui.Control.XY as XY exposing (..)
import Gui.Control.Text as Text exposing (..)
import Gui.Control.Color as Color exposing (..)
import Gui.Control.Toggle as Toggle exposing (..)
import Gui.Control.Nest as Nest exposing (..)

import Gui.Style.CellShape exposing (CellShape)


type FocusAt = FocusAt Int


type alias Label = String


type Property msg
    = Nil
    | Number (Number.Control msg)
    | Coordinate (XY.Control msg)
    | Text (Text.Control msg)
    | Color (Color.Control msg)
    | Toggle (Toggle.Control msg)
    | Action (Button.Control msg)
    | Choice (Maybe FocusAt) (Nest.ChoiceControl ( Label, Property msg ) msg)
    | Group (Maybe FocusAt) (Nest.GroupControl ( Label, Property msg ) msg)


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
                        Choice _ control ->
                            control
                                |> Nest.get index
                                |> Maybe.andThen (helper pathTail)
                        Group _ control ->
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
                        Choice _ control ->
                            control
                                |> Nest.get index
                                |> Maybe.map (helper pathTail)
                                |> Maybe.withDefault []
                        Group  _ control ->
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
        Choice focus control ->
            Choice
                focus
                <| (control
                    |> Nest.mapItems (Tuple.mapSecond <| map f)
                    |> Control.map f)
        Group focus control ->
            Group
                focus
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
                Choice _ control ->
                    f curPath item
                        <| foldItems curPath (Nest.getItems control) val
                Group _ control ->
                    f curPath item
                        <| foldItems curPath (Nest.getItems control) val
                _ -> f curPath item val

    in
        helper Path.start root from


unfold : Property msg -> List (Path, Property msg)
unfold =
    fold (\path prop prev -> ( path, prop ) :: prev ) []


mapReplace : (Path -> Property msg -> Property msg) -> Property msg -> Property msg
mapReplace f root =
    let

        replaceItem : Path -> Int -> ( Label, Property msg ) -> ( Label, Property msg )
        replaceItem parentPath index ( label, innerItem ) =
            ( label
            , helper (parentPath |> Path.advance index) innerItem
            )

        helper : Path -> Property msg -> Property msg
        helper curPath item =
            case item of
                Choice focus control ->
                    f curPath
                        <| Choice
                            focus
                        <| (control |> Nest.indexedMapItems (replaceItem curPath))
                Group focus control ->
                    f curPath
                        <| Group
                            focus
                        <| (control |> Nest.indexedMapItems (replaceItem curPath))
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
            Just <| Toggle <| Toggle.toggle toggleControl
        Action control ->
            -- we don't update the value since it's `()`, but we do execute it
            Just <| Action control
        Text textControl ->
            Just <| Text <| ensureEditing textControl
        Choice focus control ->
            Just
                <| Choice focus
                <| Nest.execute control
        Group focus control ->
            Just
                <| Group focus
                <| Nest.execute control
        _ -> Nothing


executeAt : Path -> Property msg -> List ( Path, Property msg )
executeAt path root =
    case root
        |> findWithParent path of
        Just ( parent, item ) ->
            case ( parent, item ) of
                ( Choice focus control, Action _ ) ->

                    case Path.pop path of
                        Just ( toParent, selectedIndex ) ->
                            let
                                newParent =
                                    select selectedIndex control
                            in
                                case execute item of
                                    Just newCell ->
                                        [ ( toParent, Choice focus newParent )
                                        , ( path, newCell )
                                        ]
                                    Nothing ->
                                        [ ( toParent, Choice focus newParent )
                                        ]
                        Nothing ->
                            []

                ( _, _ ) ->

                    case execute item of
                        Just newCell -> [ ( path, newCell ) ]
                        Nothing -> []

        Nothing -> []



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


expand : Property msg -> Property msg
expand prop =
    case prop of
        Group focus control ->
            Group focus <| Nest.expand control
        Choice focus control ->
            Choice focus <| Nest.expand control
        _ -> prop

collapse : Property msg -> Property msg
collapse prop =
    case prop of
        Group focus control ->
            Group focus <| Nest.collapse control
        Choice focus control ->
            Choice focus <| Nest.collapse control
        _ -> prop


detach : Property msg -> Property msg
detach prop =
    case prop of
        Group focus control ->
            Group focus <| Nest.detach control
        Choice focus control ->
            Choice focus <| Nest.detach control
        _ -> prop


expandAt : Path -> Property msg -> Property msg
expandAt path =
    updateAt path expand


detachAt : Path -> Property msg -> Property msg
detachAt path =
    updateAt path detach


detachAll : Property msg -> Property msg
detachAll =
    mapReplace <| always detach


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
        Choice _ control -> Control.call control
        Group _ control -> Control.call control


withItem
     : Int
    -> (Property msg -> Property msg)
    -> Core.Control ( a, Array ( b, Property msg ) ) value msg
    -> Core.Control ( a, Array ( b, Property msg ) ) value msg
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


getCellShape : Property msg -> Maybe CellShape
getCellShape prop =
    case prop of
        Choice _ (Control ( ( _, cellShape ), _ ) _ _ ) ->
            Just cellShape
        Group _ (Control ( ( _, cellShape ), _ ) _ _ ) ->
            Just cellShape
        _ -> Nothing


getSelected : Property msg -> Maybe ( Label, Property msg )
getSelected prop =
    case prop of
        Choice _ control ->
            control |> Nest.getSelected
        _ -> Nothing


isSelected : Property msg -> Int -> Bool
isSelected prop index =
    case prop of
        Choice _ control ->
            Nest.isSelected control index
        _ -> False
