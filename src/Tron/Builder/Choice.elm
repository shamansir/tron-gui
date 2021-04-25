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
import Json.Decode exposing (index)


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
    -> List ( Label, Property a )
    -> a
    -> ( a -> a -> Bool )
    -> ( ( Int, a ) -> msg )
    -> Property msg
helper ( panelShape, cellShape ) options current compare toMsg =
    let

        optionsArray : Array.Array ( Label, Property ( Int, a ))
        optionsArray =
            options
                |> Array.fromList
                |> Array.indexedMap
                    (\index (label, prop) ->
                        ( label
                        , prop
                            |> Tron.Property.map (Tuple.pair index)
                        )
                    )

        properties : Array.Array ( Label, Property msg )
        properties =
            optionsArray
                |> Array.map (Tuple.mapSecond <| Tron.Property.map toMsg)

        values : Array.Array ( Maybe a )
        values =
            options
                |> Array.fromList
                |> Array.map (Tuple.second >> Tron.Property.evaluate__)

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

        callByIndex : Int -> msg
        callByIndex index =
            values
                |> Array.get index
                |> Maybe.andThen identity
                |> Maybe.map (\v -> toMsg (index, v))
                |> Maybe.withDefault (toMsg ( 0, current ))
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
                , selected = currentIndex
                }
            <| Just (.selected >> callByIndex)
