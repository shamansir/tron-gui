module Tron.Builder.Choice exposing (..)


import Array


import Tron.Deferred as Def
import Tron.Property as Property exposing (..)
import Tron.Control exposing (Control(..))
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape exposing (..)
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url(..))
import Tron.Control.Nest exposing (Form(..))
import Tron.Util exposing (findMap)
import Tron.Control.Value as Value exposing (Value(..))
import Json.Decode exposing (index)


withButtons
    :  (a -> Label)
    -> (a -> Button.Face)
    -> ( ( Int -> a ) -> Int -> a -> ( Label, Property a ) )
withButtons toLabel toButtonFace =
    \callByIndex index val ->
        ( toLabel val
        , Action
            <| Control
                (toButtonFace val)
                ()
            <| callByIndex index
        )



helper
     : ( PanelShape, CellShape )
    -> List ( Label, Property a )
    -> a
    -> ( a -> a -> Bool )
    -> Property (Int, a)
helper ( panelShape, cellShape ) options current compare =
    let

        optionsArray : Array.Array ( Label, Property ( Int, a ))
        optionsArray =
            options
                |> Array.fromList
                |> Array.indexedMap
                    (\index (label, prop) ->
                        ( label
                        , prop
                            |> Property.map (Tuple.pair index)
                        )
                    )

        values : Array.Array ( Maybe a )
        values =
            options
                |> Array.fromList
                |> Array.map (Tuple.second >> Property.get)

        currentIndex : Int
        currentIndex =
            values
                |> Array.indexedMap Tuple.pair
                |> Tron.Util.filterMapArray Tron.Util.flipMaybe
                |> Tron.Util.findMapInArray
                    (\(index, option) ->
                        if compare option current
                            then Just index
                            else Nothing
                    )
                |> Maybe.withDefault 0

    in
        Choice
            Nothing
            ( panelShape
            , cellShape
            )
            <| Control
                optionsArray
                { form = Collapsed
                , page = 0
                , face = Nothing
                , selected = currentIndex
                }
                ( currentIndex, current )


helperDef
     : ( PanelShape, CellShape )
    -> List ( Label, Property ( Value -> Maybe a ) )
    -> a
    -> ( a -> a -> Bool )
    -> ( Int -> a -> msg )
    -> Property (Value -> Maybe msg)
helperDef ( panelShape, cellShape ) options current compare toMsg =
    let

        optionsArray : Array.Array ( Label, Property ( Int, ( Value -> Maybe a ) ))
        optionsArray =
            options
                |> Array.fromList
                |> Array.indexedMap
                    (\index (label, prop) ->
                        ( label
                        , prop
                            |> Property.map (Tuple.pair index)
                        )
                    )

        properties : Array.Array ( Label, Property ( Value -> Maybe msg ) )
        properties =
            optionsArray
                |> Array.indexedMap
                    (\idx (label, prop) ->
                        (label, prop |> Property.map Tuple.second |> Def.map (toMsg idx)))

        values : Array.Array ( Maybe a )
        values =
            options
                |> Array.fromList
                |> Array.map (Tuple.second)
                |> Array.map
                    (\prop ->
                        Property.get prop
                            |> Maybe.andThen (\handler_ -> handler_ <| Value.get prop)
                    )

        currentIndex : Int
        currentIndex =
            values
                |> Array.indexedMap Tuple.pair
                |> Tron.Util.filterMapArray Tron.Util.flipMaybe
                |> Tron.Util.findMapInArray
                    (\(index, option) ->
                        if compare option current
                            then Just index
                            else Nothing
                    )
                |> Maybe.withDefault 0

        handler : Value -> Maybe msg
        handler =
            Value.fromChoice
                >> Maybe.andThen
                    (\id -> Array.get id values
                                |> Maybe.andThen identity
                                |> Maybe.map (Tuple.pair id))
                >> Maybe.map (\(idx, v) -> toMsg idx v)

    in
        Choice
            Nothing
            ( panelShape
            , cellShape
            )
            <| Control
                properties
                { form = Collapsed
                , page = 0
                , face = Nothing
                , selected = currentIndex
                }
                handler


helperProxy
     : ( PanelShape, CellShape )
    -> List ( Label, Property ( Value -> Maybe a ) )
    -> a
    -> ( a -> a -> Bool )
    -> Property (Value -> Maybe Value)
helperProxy ( panelShape, cellShape ) options current compare =
    let

        optionsArray : Array.Array ( Label, Property ( Int, ( Value -> Maybe a ) ))
        optionsArray =
            options
                |> Array.fromList
                |> Array.indexedMap
                    (\index (label, prop) ->
                        ( label
                        , prop
                            |> Property.map (Tuple.pair index)
                        )
                    )

        properties : Array.Array ( Label, Property ( Value -> Maybe Value ) )
        properties =
            optionsArray
                |> Array.map
                    (\(label, prop) ->
                        (label, prop |> Property.map Tuple.second |> Property.map (always Just))
                    )

        values : Array.Array ( Maybe a )
        values =
            options
                |> Array.fromList
                |> Array.map (Tuple.second)
                |> Array.map
                    (\prop ->
                        Property.get prop
                            |> Maybe.andThen (\handler_ -> handler_ <| Value.get prop)
                    )

        currentIndex : Int
        currentIndex =
            values
                |> Array.indexedMap Tuple.pair
                |> Tron.Util.filterMapArray Tron.Util.flipMaybe
                |> Tron.Util.findMapInArray
                    (\(index, option) ->
                        if compare option current
                            then Just index
                            else Nothing
                    )
                |> Maybe.withDefault 0

    in
        Choice
            Nothing
            ( panelShape
            , cellShape
            )
            <| Control
                properties
                { form = Collapsed
                , page = 0
                , face = Nothing
                , selected = currentIndex
                }
                Just