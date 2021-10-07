module Tron.Property.Paths exposing (..)


import Dict as Dict exposing (Dict)
import Array as Array

import Tron.Property exposing (Property(..), map, map2, fold, proxify)
import Tron.Path as Path exposing (Path)
import Tron.Property.ExposeData as Exp

import Tron.Control.Nest as Nest
import Tron.Control as Control


-- Recursively try to find the control in the tree, following the given path.
-- When found and the path is valid, respond with the inner control.
-- When the path is invalid (no controls located following these indices), return `Nothing`.
find : Path -> Property a -> Maybe (Property a)
find path =
    find1 path
        >> Maybe.map Tuple.second


{- FIXME: finds by index, should use labels? -}
find1 : Path -> Property a -> Maybe (Path.Label, Property a)
find1 path root = -- TODO: reuse `fildAll` + `tail`?
    let
        helper ipath ( label, prop ) =
            case ipath of
                [] -> Just ( label, prop )
                (index, _)::pathTail ->
                    case prop of
                        Choice _ _ control ->
                            control
                                |> Nest.get index
                                |> Maybe.andThen (helper pathTail)
                        Group _ _ control ->
                            control
                                |> Nest.get index
                                |> Maybe.andThen (helper pathTail)
                        -- Live innerProp ->
                        --     helper ipath (label, innerProp)
                        _ -> Nothing
    in
        helper (Path.toList path) ( "", root )


findWithParent : Path -> Property a -> Maybe ( Property a, Property a )
findWithParent path =
    findWithParent1 path >> Maybe.map (Tuple.mapBoth Tuple.second Tuple.second)


findWithParent1 : Path -> Property a -> Maybe ( (Path.Label, Property a), (Path.Label, Property a) )
findWithParent1 path root =
    let
        allArray = findAll path root |> Array.fromList
    in
        Maybe.map2
            Tuple.pair
            (allArray |> Array.get (Array.length allArray - 2))
            (allArray |> Array.get (Array.length allArray - 1))


{- FIXME: finds by index, should use labels? -}
findAll : Path -> Property a -> List (Path.Label, Property a)
findAll path root =
    let
        helper ipath ( label, prop ) =
            ( label, prop ) :: case ipath of
                [] -> []
                (index, _)::pathTail ->
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
                        -- Live innerProp ->
                        --     helper ipath (label, innerProp)
                        _ -> [ ]
    in
        helper (Path.toList path) ( "", root )


pathifyFrom : Path -> Property a -> Property Path
pathifyFrom from root =
    -- FIXME: should be just another `fold` actually?
    let
        pathifyItem : Path -> Int -> ( Path.Label, Property a ) -> ( Path.Label, Property (Path, a) )
        pathifyItem parentPath index ( label, innerItem ) =
            ( label
            , helper (parentPath |> Path.advance ( index, label )) innerItem
            )

        helper : Path -> Property a -> Property ( Path, a )
        helper curPath item =
            case item of

                Choice focus shape control ->
                    Choice
                        focus
                        shape
                        (control
                            |> Nest.indexedMapItems (pathifyItem curPath)
                            |> Control.map (Tuple.pair curPath)
                        )

                Group focus shape control ->
                    Group
                        focus
                        shape
                        (control
                            |> Nest.indexedMapItems (pathifyItem curPath)
                            |> Control.map (Tuple.pair curPath)
                        )

                Live innerProp ->
                    Live <| helper curPath innerProp

                prop -> map (Tuple.pair curPath) prop

    in
        helper from root |> map Tuple.first


pathify : Property a -> Property Path
pathify =
    pathifyFrom Path.start


pathifyWithValue : Property a -> Property ( Path, a )
pathifyWithValue prop =
    map2
        Tuple.pair
        (pathify prop)
        prop


getPathsMappingByIndex : Property a -> Dict (List Path.Index) (List Path.Label)
getPathsMappingByIndex =
    getPathsMappingByIndex_ >> Dict.map (always Path.toLabelPath)


getPathsMappingByIndex_ : Property a -> Dict (List Path.Index) Path
getPathsMappingByIndex_ =
    let
        storePath path _ =
            Dict.insert (Path.toIndexPath path) path
    in
        fold storePath Dict.empty


getPathsMappingByLabels : Property a -> Dict (List Path.Label) (List Path.Index)
getPathsMappingByLabels =
    getPathsMappingByLabels_ >> Dict.map (always Path.toIndexPath)


getPathsMappingByLabels_ : Property a -> Dict (List Path.Label) Path
getPathsMappingByLabels_ =
    let
        storePath path _ =
            Dict.insert (Path.toLabelPath path) path
    in
        fold storePath Dict.empty


findPath : List Path.Label -> Property a -> Maybe Path
findPath labelPath =
    getPathsMappingByLabels_ -- FIXME: use `replace/fold`?
        >> Dict.get labelPath


changeLabel : Path -> String -> Property a -> Property a
changeLabel path newLabel  =
    let
        inNest curPath control =
            case Path.pop curPath of
                Just ( before, ( idx, _) ) -> -- FIXME: search not by index but by label
                    if Path.howDeep curPath == 1 then
                        Nest.withItem idx (\(_, prop) -> (newLabel, prop))
                        <| control
                    else
                        Nest.withItem idx
                            (\(label, prop) ->
                                (label, helper before prop)
                            )
                        <| control
                Nothing -> control
        helper : Path -> Property a -> Property a
        helper curPath current =
            case current of
                Choice focus shape control ->
                    Choice focus shape <| inNest curPath <| control
                Group focus shape control ->
                    Group focus shape <| inNest curPath <| control
                _ -> current
    in
        helper (path |> Path.reverse)





findByLabelPath : List Path.Label -> Property a -> Maybe (Property a)
findByLabelPath labelPath tree =
    findPath labelPath tree
        |> Maybe.andThen (\path -> find path tree)


{-| Store a `RawOutUpdate` together with message, which is a package that stores all
the required information about the value, such as:

- the path to it in the Tree, both with labels and integer IDs;
- the value in JSON;
- the value as a string;
- the type of the value, as a string;
- client ID, for the communication with WebSockets; (will be removed in future versions)

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
expose : Property a -> Property Exp.Value
expose prop =
    map2
        Tuple.pair
        (pathify prop)
        (proxify prop)
    |> map
        (\( path, proxyVal ) ->
            Exp.toRaw path proxyVal
        )