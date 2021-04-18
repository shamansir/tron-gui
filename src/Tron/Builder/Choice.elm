module Tron.Builder.Choice exposing (..)


import Array


import Tron.Property exposing (..)
import Tron.Control exposing (Control(..))
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape exposing (..)
import Tron.Control.Button as Button exposing (Face(..), Icon(..), Url(..))
import Tron.Control.Nest exposing (Form(..))
import Tron.Util exposing (findMap)
import Tron.Expose.ProxyValue exposing (ProxyValue(..))


withButtons
    :  (a -> Label)
    -> (a -> Button.Face)
    -> ( (Int -> msg ) -> Int -> a -> ( Label, Property msg ) )
withButtons toLabel toButtonFace =
    \callByIndex index val ->
        ( toLabel val
        , Action
            <| Control
                (toButtonFace val)
                ()
            <| Just
            <| always
            <| callByIndex index
        )



helper
     : ( PanelShape, CellShape )
    -> ( (Int -> msg) -> Int -> a -> ( Label, Property msg ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Property msg
helper ( panelShape, cellShape ) toBuilder options current compare toMsg =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex indexToCall =
            -- FIXME: searching for the item every time seems wrong, use Array as a backup?
            indexedOptions
                |> findMap
                    (\(index, option) ->
                        if indexToCall == index
                            then Just option
                            else Nothing
                    )
                |> Maybe.map toMsg
                |> Maybe.withDefault (toMsg current)
    in
        Choice
            Nothing
            ( panelShape
            , cellShape
            )
            <| Control
                ( options
                    |> List.indexedMap (toBuilder callByIndex)
                    |> Array.fromList
                )
                { form = Collapsed
                , page = 0
                , selected =
                    indexedOptions
                    -- FIXME: searching for the item every time seems wrong, use Array as a backup?
                        |> findMap
                            (\(index, option) ->
                                if compare option current
                                    then Just index
                                    else Nothing
                            )
                        |> Maybe.withDefault 0
                }
                (Just <| .selected >> callByIndex)


{-| -}
proxyHelper
     : ( PanelShape, CellShape )
    -> ( (Int -> ProxyValue) -> Int -> a -> ( Label, Property ProxyValue ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Property ProxyValue
proxyHelper ( panelShape, cellShape ) toBuilder options current compare =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex indexToCall =
            FromChoice indexToCall
        set =
            options
                |> List.indexedMap (toBuilder callByIndex)
    in
        Choice
            Nothing
            ( panelShape
            , cellShape
            )
            <| Control
                ( set |> Array.fromList )
                { form = Collapsed
                , page = 0
                , selected =
                    indexedOptions
                        -- FIXME: searching for the item every time seems wrong
                        |> findMap
                            (\(index, option) ->
                                if compare option current
                                    then Just index
                                    else Nothing
                            )
                        |> Maybe.withDefault 0
                }
                (Just <| .selected >> callByIndex)
