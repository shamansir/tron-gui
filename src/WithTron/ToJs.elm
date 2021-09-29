module WithTron.ToJs exposing
    ( toJs, toJsApp
    )


{-| `Backed` programs are the ones that have a permanent storage for the interface values behind the API.

This is mostly useful for the programs that heavily interact with JS side, or even only the Tron GUI is there on Elm side and the main code is in JS. So you do not want to specify your own model or messages, but only the GUI and nothing else.

That way, we only ask for `Tron ()` interface definition, with no specific messages required as handler, and `Tron.Builder.Unit` module has all the useful helpers for this, whose functions require no handlers, since no messages are sent.

See different `Backed...` examples in the `example` folder, and also `ForTiler`.

Some helpers below have the access to such storage using `ValueAt` function being passed to the
lifecycle of the app. This is for the cases when you need, for example, to show/hide some parts of your interface still, w/o specifying any model.

# JSON

@docs byJson, BackedByJson

# Strings

@docs byStrings, BackedByStrings

# Proxy

@docs byProxy, BackedByProxy

# as the Application

@docs byProxyApp, AppBackedByProxy

See also `WithTron.ValueAt`.
-}


import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (Html)

import Tron exposing (Tron)
import Tron.OfValue as Def
import WithTron exposing (..)
import WithTron.ValueAt exposing (ValueAt)

import Tron.Path as Path exposing (Path)
import Tron.Option.RenderTarget exposing (..)
import Tron.Option.Communication exposing (..)
import Tron.Expose as Exp
import Tron.Expose.Convert as Exp
import Tron.Expose.Data as Exp
import Tron.Property as Property exposing (LabelPath)
import Tron.Control.Value exposing (Value)


{-| Path-to-value storage, to transmit values to the JS side. -}
type alias ValueStorage = Dict LabelPath Value


type alias ValueUpdate = (List Path.Index, LabelPath, Value)


{-| Program, backed with the path-to-value storage. -}
type alias ProgramWithJsCommunication flags model msg
    = ProgramWithTron flags ( model, ValueStorage ) ( msg, ValueUpdate )



{-| Currently, the only way (okay, including `byProxyApp`, which is actually the same) to get access to the values in the UI, while using `Backed` way of communication.

See `WithTron.ValueAt` for more information and `example/ForTiler` as the example.
-}
toJs
    :  RenderTarget
    ->
        ( Exp.Property -> Cmd msg
        , Exp.Out -> Cmd msg
        , Sub (List Exp.DeduceIn)
        )
    -> (ValueAt -> Tron ())
    -> BackedByProxy
toJs renderTarget ( ack, transmit, apply ) for =
    toJsApp
        renderTarget
        ( ack >> Cmd.map (always ())
        , transmit >> Cmd.map (always ())
        , apply
        )
        { for = \valueAt _ -> for valueAt
        , init = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , view = \_ _ -> Html.div [] []
        , update = \_ _ _ -> ( (), Cmd.none )
        }


{-| Get access to the current values in UI, using `Backed` to send values to JS and still having
your own model and application flow, separated from those values.

This is for the case when, indeed, you have some simple `Model`, don't want to bother with storing UI values and/or accessing them using `LabelPath` is enough for you.

See `WithTron.ValueAt` for more information and `example/ForTiler` as the example.
-}

toJsApp
    :  RenderTarget
    ->
        ( Exp.Property -> Cmd msg
        , Exp.Out -> Cmd msg
        , Sub (List Exp.DeduceIn)
        )
    ->  { for : ValueAt -> model -> Tron msg
        , init : flags -> ValueAt -> ( model, Cmd msg )
        , subscriptions : ValueAt -> model -> Sub msg
        , view : ValueAt -> model -> Html msg
        , update : msg -> ValueAt -> model -> ( model, Cmd msg )
        }
    -> ProgramWithJsCommunication flags model msg
toJsApp renderTarget ( ack, transmit, apply ) def =
    let

        valueAt dict =
            \path -> Dict.get path dict

        toValueAt : ValueStorage -> ValueAt
        toValueAt storage =
            storage |> valueAt

        repair ( ( path, labelPath ), ( proxy, userMsg ) ) =
            ( Just ( Path.toList path, labelPath, proxy ), userMsg )

        for_ : ( model, ValueStorage ) -> Def.Tron ( msg, ValueUpdate )
        for_ ( model, storage ) =
            def.for (toValueAt storage) model
                |> Exp.loadValues ( dictByPath dict )
                |> Property.addPaths
                |> Tron.map
                    (\((path, labelPath), userMsg) ->
                        \proxy ->
                            Just ( Just ( Path.toList path, labelPath, proxy ), userMsg )
                    )

        init_ : flags -> ( ( model, ValueStorage ), Cmd ( msg, ValueUpdate ) )
        init_ flags =
            let
                storage = Dict.empty
                ( userModel, userEff ) =
                    def.init flags (toValueAt storage)
            in
                ( ( userModel, storage )
                , userEff
                    |> Cmd.map (Tuple.pair Nothing)
                )

        update_
            :  ( msg, ValueUpdate )
            -> ( model, ValueStorage )
            -> ( ( model, ValueStorage ), Cmd ( msg, ValueUpdate ) )
        update_ ( storageUpdate, userMsg ) ( storage, userModel ) =
            let
                nextStorage =
                    case storageUpdate of
                        Just ( rawPath, labelPath, proxy )
                            -> storage
                                    |> Dict.insert
                                        ( rawPath, labelPath )
                                        proxy
                        Nothing -> storage
                ( nextUserModel, nextUserEffects )
                    = def.update userMsg (toValueAt nextStorage) userModel
            in
                ( ( nextStorage, nextUserModel )
                , nextUserEffects
                    |> Cmd.map (Tuple.pair Nothing)
                )

        view_ : ( model, ValueStorage ) -> Html ( msg, ValueUpdate )
        view_ ( storage, model ) =
            def.view (toValueAt storage) model
                |> Html.map (Tuple.pair Nothing)


        subscriptions_ : ( model, ValueStorage ) -> Sub ( msg, ValueUpdate )
        subscriptions_ ( storage, model ) =
            def.subscriptions (toValueAt storage) model
                |> Sub.map (Tuple.pair Nothing)

    in
    element
        renderTarget
        (SendReceiveJson
            { ack = ack >> Cmd.map (Tuple.pair Nothing)
            , transmit = transmit >> Cmd.map (Tuple.pair Nothing)
            , apply = apply
            }
        )
        { for = for_
        , init = init_
        , update = update_
        , view = view_
        , subscriptions = subscriptions_
        }
