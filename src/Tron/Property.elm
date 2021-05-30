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
import Tron.Util as Util

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


knobDistance = 90


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
    : (Maybe a -> Maybe b -> c)
    -> Property a
    -> Property b
    -> Property c
zip f propA propB =
    move ()
-}


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


fold1 : (a -> x) -> Property a -> Maybe x
fold1 f prop =
    case prop of
        Nil -> Nothing
        Number control -> control |> Control.fold f |> Just
        Coordinate control -> control |> Control.fold f |> Just
        Text control -> control |> Control.fold f |> Just
        Color control -> control |> Control.fold f |> Just
        Toggle control -> control |> Control.fold f |> Just
        Action control -> control |> Control.fold f |> Just
        Choice _ _ control -> control |> Control.fold f |> Just
        Group _ _ control -> control |> Control.fold f |> Just



andThen : (a -> Property b) -> Property a -> Property b
-- FIXME: should be changed to `andThen` with getting rid of function in Control
andThen f =
    fold1 f >> Maybe.withDefault Nil


with : (a -> Property a -> Property b) -> Property a -> Property b
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
            case Nest.getChoiceMode control of
                Nest.Pages ->
                    Just
                        <| Choice focus shape
                        <| Nest.toggle control
                Nest.Knob ->
                    Just
                        <| Choice focus shape
                        <| Nest.toNext control
                Nest.SwitchThrough ->
                    Just
                        <| Choice focus shape
                        <| Nest.toNext control
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


transferTransientState : Property a -> Property b -> Property b
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
    in move f propA propB


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


isExpanded : Property a -> Maybe Nest.Form
isExpanded prop =
    case prop of
        Group _ _ control ->
            Just <| Nest.getForm control
        Choice _ _ control ->
            Just <| Nest.getForm control
        _ -> Nothing


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


setChoiceMode : Nest.ChoiceMode -> Property a -> Property a
setChoiceMode newMode prop =
    case prop of
        Choice focus shape control ->
            Choice focus shape
                <| Nest.setChoiceMode newMode
                <| control
        _ -> prop


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


toChoice : Property a -> Property a
toChoice prop =
    case prop of
        Group focus shape control ->
            Choice focus shape
                --<| Control.mapByValue (.selected >> f)
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


compareValues : Property a -> Property b -> Bool
compareValues propA propB =
    case (propA, propB) of
        (Nil, Nil) -> True
        (Number controlA, Number controlB) ->
            Tuple.second (Control.getValue controlA) == Tuple.second (Control.getValue controlB)
        (Coordinate controlA, Coordinate controlB) ->
            Tuple.second (Control.getValue controlA) == Tuple.second (Control.getValue controlB)
        (Text controlA, Text controlB) ->
            Control.getValue controlA == Control.getValue controlB
        (Color controlA, Color controlB) ->
            Tuple.second (Control.getValue controlA) == Tuple.second (Control.getValue controlB)
        (Toggle controlA, Toggle controlB) ->
            Control.getValue controlA == Control.getValue controlB
        (Action _, Action _) -> True
        (Choice _ _ controlA, Choice _ _ controlB) ->
            Nest.whichSelected controlA == Nest.whichSelected controlB
        (Group _ _ _, Group _ _ _) -> True
        (_, _) -> False


fold2_ : ((Maybe (Property a), Maybe (Property b)) -> c -> c) -> Property a -> Property b -> c -> c
fold2_ f propA propB = fold2Helper f ( Just propA, Just propB )


fold2 : ((Maybe a, Maybe b) -> c -> c) -> Property a -> Property b -> c -> c
fold2 f =
    fold2_
        (\(pA, pB) ->
            f
                ( (pA |> Maybe.andThen get)
                , (pB |> Maybe.andThen get)
                )
        )


fold2Helper : ((Maybe (Property a), Maybe (Property b)) -> c -> c) -> ( Maybe (Property a), Maybe (Property b) ) -> c -> c
fold2Helper f ( maybePropA, maybePropB ) def =
    let
        foldNestItems itemsA itemsB =
            Util.zipArrays
                (itemsA |> Array.map Tuple.second)
                (itemsB |> Array.map Tuple.second)
                |> Array.foldl (fold2Helper f) def
                |> f ( maybePropA, maybePropB )

    in case ( maybePropA, maybePropB ) of

        ( Just (Group _ _ groupControlA), Just (Group _ _ groupControlB) ) ->
            foldNestItems
                (Nest.getItems groupControlA)
                (Nest.getItems groupControlB)

        ( Just (Choice _ _ choiceControlA), Just (Choice _ _ choiceControlB) ) ->
            foldNestItems
                (Nest.getItems choiceControlA)
                (Nest.getItems choiceControlB)

        ( Just (Group _ _ groupControlA), _ ) ->
            foldNestItems
                (Nest.getItems groupControlA)
                Array.empty

        ( Just (Choice _ _ choiceControlA), _ ) ->
            foldNestItems
                (Nest.getItems choiceControlA)
                Array.empty

        ( _, Just (Group _ _ groupControlB) ) ->
            foldNestItems
                Array.empty
                (Nest.getItems groupControlB)

        ( _, Just (Choice _ _ choiceControlB) ) ->
            foldNestItems
                Array.empty
                (Nest.getItems choiceControlB)

        ( _, _ ) -> f ( maybePropA, maybePropB ) def


move : (Property a -> Property b -> Property c) -> Property a -> Property b -> Property c
move f propA propB =
    let
        merge ( maybeA, maybeB ) =
            case ( maybeA, maybeB ) of
                ( Just (labelA, propA_), Just (labelB, propB_) ) ->
                    (labelB, move f propA_ propB_)
                    --Just (labelB, move f propA_ propB_)
                ( Nothing, Just (labelB, propB_) ) ->
                    (labelB, move f Nil propB_)
                    --Just (labelB, move f Nil propB_)
                ( Just (labelA, propA_), Nothing ) ->
                    --Nothing
                    (labelA, move f propA Nil)
                ( Nothing, Nothing ) ->
                    --Nothing
                    ("nil", move f Nil Nil)
        zipItems controlA controlB =
            Util.zipArrays
                (Nest.getItems controlA)
                (Nest.getItems controlB)
                |> Array.map merge
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


setValueTo : Property a -> Property b -> Property b -- FIXME: return `Maybe`
setValueTo from to =
    case ( from, to ) of
        (Number controlA, Number controlB) ->
            Number <| Control.setValue (Control.getValue controlA) <| controlB
        (Coordinate controlA, Coordinate controlB) ->
            Coordinate <| Control.setValue (Control.getValue controlA) <| controlB
        (Text controlA, Text controlB) ->
            Text <| Control.setValue (Control.getValue controlA) <| controlB
        (Color controlA, Color controlB) ->
            Color <| Control.setValue (Control.getValue controlA) <| controlB
        (Toggle controlA, Toggle controlB) ->
            Toggle <| Control.setValue (Control.getValue controlA) <| controlB
        (Action controlA, Action controlB) ->
            Action <| Control.setValue (Control.getValue controlA) <| controlB
        (Choice _ _ controlA, Choice focus shape controlB) ->
            Choice focus shape <| Control.setValue (Control.getValue controlA) <| controlB
        (Group _ _ controlA, Group focus shape controlB) ->
            Group focus shape <| Control.setValue (Control.getValue controlA) <| controlB
        (_, _) -> to



-- map2 use `move` for that


loadValues : Property a -> Property b -> Property b
loadValues = move setValueTo