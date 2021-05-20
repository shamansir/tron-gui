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

import Tron.Pages as Pages exposing (Pages)
import Size exposing (..)

import Tron.Style.CellShape as CS exposing (CellShape)
import Tron.Style.PanelShape as PS exposing (PanelShape)


type FocusAt = FocusAt Int


type alias Shape = ( Float, Float )


type alias NestShape = ( PanelShape, CellShape )


type alias Label = String


type alias LabelPath = List String


type Property a
    = Nil
    | Number (Number.Control a)
    | Coordinate (XY.Control a)
    | Text (Text.Control a)
    | Color (Color.Control a)
    | Toggle (Toggle.Control a)
    | Action (Button.Control a)
    | Choice (Maybe FocusAt) NestShape (Nest.ChoiceControl ( Label, Property a ) a)
    | Group (Maybe FocusAt) NestShape (Nest.GroupControl ( Label, Property a ) a)


knobDistance = 90 * 4


defaultNestShape : NestShape
defaultNestShape = ( PS.auto, CS.single )



-- Recursively try to find the control in the tree, following the given path.
-- When found and the path is valid, respond with the inner control.
-- When the path is invalid (no controls located following these indices), return `Nothing`.
find : Path -> Property a -> Maybe (Property a)
find path =
    find1 path
        >> Maybe.map Tuple.second


find1 : Path -> Property a -> Maybe (Label, Property a)
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


findWithParent : Path -> Property a -> Maybe ( Property a, Property a )
findWithParent path =
    findWithParent1 path >> Maybe.map (Tuple.mapBoth Tuple.second Tuple.second)


findWithParent1 : Path -> Property a -> Maybe ( (Label, Property a), (Label, Property a) )
findWithParent1 path root =
    let
        allArray = findAll path root |> Array.fromList
    in
        Maybe.map2
            Tuple.pair
            (allArray |> Array.get (Array.length allArray - 2))
            (allArray |> Array.get (Array.length allArray - 1))


findAll : Path -> Property a -> List (Label, Property a)
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


map : (a -> b) -> Property a -> Property b
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


{- zip
    : (Property a -> Property b -> Property c)
    -> Property a
    -> Property b
    -> Property c
zip f propA propB =
    case ( propA, propB ) of
        ( Choice _ _ controlA, Choice _ _ controlB ) ->

        ( Group _ _ controlA, Group _ _ controlB ) ->
        _ -> f propA propB -}


fold : (Path -> Property a -> b -> b) -> b -> Property a -> b
fold f from root =
    let

        foldItems : Path -> Array ( Label, Property a ) -> b -> b
        foldItems curPath items val =
            items
                |> Array.map Tuple.second
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, innerItem) prev ->
                        helper (curPath |> Path.advance index) innerItem prev
                    )
                    val

        helper : Path -> Property a -> b -> b
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


unfold : Property a -> List (Path, Property a)
unfold =
    fold (\path prop prev -> ( path, prop ) :: prev ) []


andThen : (a -> Property a) -> Property a -> Property a
-- FIXME: should be changed to `andThen` with getting rid of function in Control
andThen f prop =
    case prop of
        Nil -> Nil
        Number control -> control |> Control.fold f
        Coordinate control -> control |> Control.fold f
        Text control -> control |> Control.fold f
        Color control -> control |> Control.fold f
        Toggle control -> control |> Control.fold f
        Action control -> control |> Control.fold f
        Choice _ _ control -> control |> Control.fold f
        Group _ _ control -> control |> Control.fold f



with : (a -> Property a -> Property a) -> Property a -> Property a
-- FIXME: should be changed to `andThen` with getting rid of function in Control
with f prop =
    andThen (\v -> f v prop) prop



-- `replace` -- find better name
replace : (Path -> Property a -> Property a) -> Property a -> Property a
replace = replaceMap <| always identity


replaceWithLabeledPath
    : (LabelPath -> Property a -> Property a) -> Property a -> Property a
replaceWithLabeledPath =
    replaceWithLabeledPathMap <| always identity


-- aren't `...Map` functions are compositions like `replace << map`?
replaceMap
    :  (Path -> a -> b)
    -> (Path -> Property b -> Property b)
    -> Property a
    -> Property b
replaceMap aMap f =
    replaceWithPathsMap (Tuple.first >> aMap) (Tuple.first >> f)


replaceWithLabeledPathMap
    :  (LabelPath -> a -> b)
    -> (LabelPath -> Property b -> Property b)
    -> Property a
    -> Property b
replaceWithLabeledPathMap aMap f =
    replaceWithPathsMap (Tuple.second >> aMap) (Tuple.second >> f)


replaceWithPathsMap
    :  (( Path, LabelPath ) -> a -> b)
    -> (( Path, LabelPath ) -> Property b -> Property b)
    -> Property a
    -> Property b
replaceWithPathsMap aMap f root =
    -- FIXME: should be just another `fold` actually?

    let

        replaceItem
            :  ( Path, LabelPath )
            -> Int
            -> ( Label, Property a )
            -> ( Label, Property b )
        replaceItem ( parentPath, parentLabelPath ) index ( label, innerItem ) =
            ( label
            , helper
                ( parentPath |> Path.advance index
                , parentLabelPath ++ [ label ]
                )
                innerItem
            )

        helper : ( Path, LabelPath ) -> Property a -> Property b
        helper curPath item =
            case item of
                Choice focus shape control ->
                    f curPath
                        <| Choice
                            focus
                            shape
                        <| (control
                                |> Nest.indexedMapItems (replaceItem curPath)
                                |> Control.map (aMap curPath))
                Group focus shape control ->
                    f curPath
                        <| Group
                            focus
                            shape
                        <| (control
                                |> Nest.indexedMapItems (replaceItem curPath)
                                |> Control.map (aMap curPath))
                _ -> f curPath <| map (aMap curPath) <| item

    in
        helper ( Path.start, [] )  root


addPathFrom : Path -> Property a -> Property ( Path, a )
addPathFrom from root =
    -- FIXME: should be just another `fold` actually?
    let
        replaceItem : Path -> Int -> ( Label, Property a ) -> ( Label, Property (Path, a) )
        replaceItem parentPath index ( label, innerItem ) =
            ( label
            , helper (parentPath |> Path.advance index) innerItem
            )

        helper : Path -> Property a -> Property ( Path, a )
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


addPath : Property a -> Property ( Path, a )
addPath =
    replaceMap (Tuple.pair) (always identity)


addLabeledPath : Property a -> Property ( LabelPath, a )
addLabeledPath =
    replaceWithLabeledPathMap (Tuple.pair) (always identity)


addPaths : Property a -> Property ( ( Path, LabelPath ), a )
addPaths =
    replaceWithPathsMap (Tuple.pair) (always identity)


updateAt : Path -> (Property a -> Property a) -> Property a -> Property a
updateAt path f =
    replace
        <| \otherPath item ->
            if Path.equal otherPath path then f item else item


updateMany : List ( Path, Property a ) -> Property a -> Property a
updateMany updates root =
    List.foldl
        (\(path, nextProp) lastRoot ->
            lastRoot |> updateAt path (always nextProp)
        )
        root
        updates


setAt : Path -> Property a -> Property a -> Property a
setAt path newProperty =
    updateAt path <| always newProperty


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
-- FIXME: should not call controls itself, only return the update
execute : Property a -> Maybe (Property a)
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


executeAt : Path -> Property a -> List ( Path, Property a )
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


transferTransientState : Property a -> Property a -> Property a
transferTransientState propA propB =
    let
        f propA_ propB_ =
            case ( propA_, propB_ ) of
                ( Choice focusA _ controlA, Choice _ shapeB controlB ) ->
                    Choice focusA shapeB
                        (Nest.getTransientState controlA
                            |> Nest.restoreTransientState controlB)
                ( Group focusA _ controlA, Group _ shapeB controlB ) ->
                    Group focusA shapeB
                        (Nest.getTransientState controlA
                            |> Nest.restoreTransientState controlB)
                ( Text controlA, Text controlB ) ->
                    Text
                        (Text.getTransientState controlA
                            |> Text.restoreTransientState controlB)
                _ -> propB_
        zipItems controlA controlB =
            let
                itemsA = Nest.getItems controlA
                itemsB = Nest.getItems controlB
                lengthA = Array.length itemsA
                lengthB = Array.length itemsB
            in
                List.map2
                    (\maybeA maybeB ->
                        case ( maybeA, maybeB ) of
                            ( Just (labelA, propA_), Just (labelB, propB_) ) ->
                                Just (labelB, transferTransientState propA_ propB_)
                            ( Nothing, Just (labelB, propB_) ) ->
                                Just (labelB, propB_)
                            ( _, Nothing ) ->
                                Nothing

                    )
                    (if lengthB <= lengthA
                        then itemsA |> Array.map Just |> Array.toList
                        else
                            -- if new items were added in stateB, we should
                            -- ensure to keep the new ones from B,
                            -- so we fill up itemsA with items
                            -- until it has the same length as itemsB,
                            -- or else `map2` will skip them
                            Array.append
                                (itemsA |> Array.map Just)
                                (Array.repeat (lengthB - lengthA) Nothing)
                            |> Array.toList
                    )
                    (itemsB |> Array.map Just |> Array.toList)
                    |> List.filterMap identity
                    |> Array.fromList
    in
    case ( propA, propB ) of
        ( Choice _ _ controlA, Choice _ _ controlB ) ->
            case f propA propB of
                Choice focus shape control ->
                    Choice focus shape <| Nest.setItems (zipItems controlA controlB) <| control
                otherProp -> otherProp
        ( Group _ _ controlA, Group _ _ controlB ) ->
            case f propA propB of
                Group focus shape control ->
                    Group focus shape <| Nest.setItems (zipItems controlA controlB) <| control
                otherProp -> otherProp
        _ -> f propA propB


-- TODO: better use the functions below directly from their controls


finishEditingAt : Path -> Property a -> Property a
finishEditingAt path =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text <| Text.finishEditing control
                _ -> prop


updateTextAt : Path -> String -> Property a -> Property a
updateTextAt path newValue =
    updateAt path <|
        \prop ->
            case prop of
                Text control -> Text ( control |> Text.updateText newValue )
                _ -> prop



-- updateAndExecute : (v -> v) -> Control s v a -> ( Control s v a, a )

ensureEditing : Property a -> Property a
ensureEditing prop =
    case prop of
        Text control ->
            Text <| Text.ensureEditing control
        _ -> prop


expand : Property a -> Property a
expand prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.expand control
        Choice focus shape control ->
            Choice focus shape <| Nest.expand control
        _ -> prop


collapse : Property a -> Property a
collapse prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.collapse control
        Choice focus shape control ->
            Choice focus shape <| Nest.collapse control
        _ -> prop


detach : Property a -> Property a
detach prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.detach control
        Choice focus shape control ->
            Choice focus shape <| Nest.detach control
        _ -> prop


switchPage : Pages.PageNum -> Property a -> Property a
switchPage pageNum prop =
    case prop of
        Group focus shape control ->
            Group focus shape <| Nest.switchTo pageNum <| control
        Choice focus shape control ->
            Choice focus shape <| Nest.switchTo pageNum <| control
        _ -> prop


expandAt : Path -> Property a -> Property a
expandAt path =
    updateAt path expand


detachAt : Path -> Property a -> Property a
detachAt path =
    updateAt path detach


switchPageAt : Path -> Pages.PageNum -> Property a -> Property a
switchPageAt path pageNum =
    updateAt path <| switchPage pageNum


detachAll : Property a -> Property a
detachAll =
    replace <| always detach


toggle : Property a -> Property a
toggle prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggle control
        _ -> prop


toggleAt : Path -> Property a -> Property a
toggleAt path =
    updateAt path toggle


toggleOn : Property a -> Property a
toggleOn prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggleOn control
        _ -> prop


toggleOff : Property a -> Property a
toggleOff prop =
    case prop of
        Toggle control ->
            Toggle <| Toggle.toggleOff control
        _ -> prop


ensureEditingAt : Path -> Property a -> Property a
ensureEditingAt path =
    updateAt path ensureEditing


{-
reshape : Shape -> Property a -> Property a
reshape shape prop =
    case prop of
        Group ( Control ( _, items ) ( expanded, focus ) handler ) ->
            Group ( Control ( shape, items ) ( expanded, focus ) handler )
        _ -> prop
-}


isGhost : Property a -> Bool
isGhost prop =
    case prop of
        Nil -> True
        _ -> False


noGhosts : List (Property a) -> List (Property a)
noGhosts = List.filter (not << isGhost)


reflect : Property a -> Maybe a
reflect prop =
    case prop of
        Nil -> Nothing
        Number control -> control |> Control.get |> Just
        Coordinate control -> control |> Control.get |> Just
        Text control -> control |> Control.get |> Just
        Color control -> control |> Control.get |> Just
        Toggle control -> control |> Control.get |> Just
        Action control -> control |> Control.get |> Just
        Choice _ _ control -> control |> Control.get |> Just
        Group _ _ control -> control |> Control.get |> Just


run : Property msg -> Cmd msg
run prop =
    case prop of
        Nil -> Cmd.none
        Number control -> control |> Control.run
        Coordinate control -> control |> Control.run
        Text control -> control |> Control.run
        Color control -> control |> Control.run
        Toggle control -> control |> Control.run
        Action control -> control |> Control.run
        Choice _ _ control -> control |> Control.run
        Group _ _ control -> control |> Control.run


get : Property a -> Maybe a
get prop =
    case prop of
        Nil -> Nothing
        Number control -> control |> Control.get |> Just
        Coordinate control -> control |> Control.get |> Just
        Text control -> control |> Control.get |> Just
        Color control -> control |> Control.get |> Just
        Toggle control -> control |> Control.get |> Just
        Action control -> control |> Control.get |> Just
        Choice _ _ control -> control |> Control.get |> Just
        Group _ _ control -> control |> Control.get |> Just


getCellShape : Property a -> Maybe CellShape
getCellShape prop =
    case prop of
        Choice _ ( _, cellShape ) _ ->
            Just cellShape
        Group _ ( _, cellShape ) _ ->
            Just cellShape
        _ -> Nothing


getPageNum : Property a -> Maybe Pages.PageNum
getPageNum prop =
    case prop of
        Choice _ _ control ->
            Just <| Nest.getPage control
        Group _ _ control ->
            Just <| Nest.getPage control
        _ -> Nothing


getSelected : Property a -> Maybe ( Label, Property a )
getSelected prop =
    case prop of
        Choice _ _ control ->
            control |> Nest.getSelected
        _ -> Nothing


isSelected : Property a -> Int -> Bool
isSelected prop index =
    case prop of
        Choice _ _ control ->
            Nest.isSelected control index
        _ -> False


setFace : Button.Face -> Property a -> Property a
setFace face prop =
    case prop of
        Action control ->
            Action
                <| Button.setFace face
                <| control
        Group focus shape control ->
            Group focus shape
                <| Nest.setFace face
                <| control
        Choice focus shape control ->
            Choice focus shape
                <| Nest.setFace face
                <| control
        _ -> prop


toChoice : (ItemId -> a) -> Property a -> Property a
toChoice f prop =
    case prop of
        Group focus shape control ->
            Choice focus shape
                <| Control.mapByValue (.selected >> f)
                <| Nest.toChoice
                <| control
        _ -> prop


setPanelShape : PanelShape -> Property a -> Property a
setPanelShape ps prop =
    case prop of
        Group focus ( _, cs ) control ->
            Group focus ( ps, cs ) control
        Choice focus ( _, cs ) control ->
            Choice focus ( ps, cs ) control
        _ -> prop


setCellShape : CellShape -> Property a -> Property a
setCellShape cs prop =
    case prop of
        Group focus ( ps, _ ) control ->
            Group focus ( ps, cs ) control
        Choice focus ( ps, _ ) control ->
            Choice focus ( ps, cs ) control
        _ -> prop
