module WithTron.Backed exposing
    ( ValueAt
    , byJson, BackedByJson
    , byStrings, BackedByStrings
    , byProxy, BackedByProxy
    , byProxyApp, AppBackedByProxy
    )


{-| `Backed` programs are the ones that have a permanent storage for the interface values behind the API.

This is mostly useful for the programs that heavily interact with JS side, or even only the Tron GUI is there on Elm side and the main code is in JS. So you do not want to specify your own model or messages, but only the GUI and nothing else.

That way, we only ask for `Tron ()` interface definition, with no specific messages required as handler, and `Tron.Builder.Unit` module has all the useful helpers for this, whose functions require no handlers, since no messages are sent.

See different `Backed...` examples in the `example` folder.

Some helpers below have the access to such storage using `ValueAt` function being passed to the
lifecycle of the app. This is for the cases when you need, for example, to show/hide some parts of your interface still, w/o specifying any model.
-}


import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (Html)

import Tron exposing (Tron)
import WithTron exposing (..)

import Tron.Path as Path exposing (Path)
import Tron.Option exposing (..)
import Tron.Expose as Exp
import Tron.Expose.Convert as Exp
import Tron.Expose.Data as Exp
import Tron.Property as Property exposing (LabelPath)
import Tron.Expose.ProxyValue exposing (ProxyValue(..))


{-| Path-to-value storage, to transmit them to the JS side. -}
type alias BackedStorage = Dict Exp.RawPath Exp.RawOutUpdate


type alias BackedMsg = Exp.RawOutUpdate


{-| Program, backed with the path-to-value storage. -}
type alias BackedByJson = ProgramWithTron () BackedStorage BackedMsg


{-| Special helper, that just asks you for the `Unit`-based interface and some port where to send value updates as JSON packages.

See `Tron.Builder.Unit` for the functions that will help you define the interface that only operates `()` values and so doesn't need any messages.

    import WithTron.Backed exposing (BackedByJson)
    import Tron exposing (Tron)
    import Tron.Builder.Unit exposing (..)
    import Tron.Expose.Data as Exp

    gui : Tron ()
    gui =
        Builder.root
            [
                ( "amount"
                , Builder.float
                    { min = 0, max = 1, step = 0.01 }
                    0
                )
            ]

    port transmit : Exp.RawOutUpdate -> Cmd msg
    port ack : Exp.RawProperty -> Cmd msg

    main : BackedByJson
    main =
        WithTron.Backed.byJson
            (Option.toHtml Dock.middleRight Theme.dark)
            ( ack, transmit )
            gui


See `example/ReportToJsBacked` for more details.

*NB*: Notice that using the GUI that changes its tree structure while running may lead to problems since the ID-based paths then correspond to other controls. In this case, please either prefer  `stringBacked` over `backed` (it uses label-path as a key) or always define currently-invisible controls/nestings with `Builder.none` — it creates the so-called ghost control which has it's own ID but completely hidden from the GUI;

 -}
byJson
    :  RenderTarget
    ->
        ( Exp.RawProperty -> Cmd msg
        , Exp.RawOutUpdate -> Cmd msg
        )
    -> Tron ()
    -> BackedByJson
byJson renderTarget ( ack, transmit ) tree =
    let

        tree_ : Tron BackedMsg
        tree_ = tree |> Exp.toExposed |> Property.map Tuple.first

        for_ : BackedStorage -> Tron BackedMsg
        for_ dict = tree_ |> Exp.loadJsonValues dict

        init_ : () -> ( BackedStorage, Cmd BackedMsg )
        init_ _ = ( Dict.empty, Cmd.none )

        update_
            :  BackedMsg
            -> BackedStorage
            -> ( BackedStorage, Cmd BackedMsg )
        update_ rawUpdate dict =
            ( dict
                |> Dict.insert rawUpdate.path rawUpdate
            , Cmd.none
            )

        view_ : BackedStorage -> Html BackedMsg
        view_ _ = Html.div [] []


        subscriptions_ : BackedStorage -> Sub BackedMsg
        subscriptions_ _ = Sub.none

    in
    element
        renderTarget
        (SendJson
            { ack = ack >> Cmd.map (always Exp.emptyOutUpdate)
            , transmit = transmit >> Cmd.map (always Exp.emptyOutUpdate)
            }
        )
        { for = for_
        , init = init_
        , update = update_
        , view = view_
        , subscriptions = subscriptions_
        }


{-| Path-to-value storage, to transmit them to the JS side. -}
type alias StringBackedStorage = Dict LabelPath String


type alias StringBackedMsg = ( LabelPath, String ) -- message is just key & value to put in the dict


{-| Program, backed with the path-to-value storage. -}
type alias BackedByStrings = ProgramWithTron () StringBackedStorage StringBackedMsg


{-| Special helper, that just asks you for the `Unit`-based interface and some port where to send value updates as `(String, String) pairs, which are labeled path and the stringified value.

See `Tron.Builder.Unit` for the functions that will help you define the interface that only operates `()` values and so doesn't need any messages.

    import WithTron.Backed exposing (BackedByStrings)
    import Tron exposing (Tron)
    import Tron.Builder.Unit exposing (..)

    gui : Tron ()
    gui =
        Builder.root
            [
                ( "amount"
                , Builder.float
                    { min = 0, max = 1, step = 0.01 }
                    0
                )
            ]

    port sendUpdate : ( String, String ) -> Cmd msg

    main : BackedByStringsWithTron
    main =
        WtihTron.Backed.byStrings
            (Option.toHtml Dock.middleRight Theme.dark)
            sendUpdate
            gui


See `example/ReportToJsStringBacked` for more details.

 -}
byStrings
    :  RenderTarget
    -> (( List String, String ) -> Cmd msg)
    -> Tron ()
    -> BackedByStrings
byStrings renderTarget transmit tree =
    let

        tree_ : Tron StringBackedMsg
        tree_ = tree |> Exp.toStrExposed |> Property.map Tuple.first

        for_ : StringBackedStorage -> Tron StringBackedMsg
        for_ dict = tree_ |> Exp.loadValues dict

        init_ : () -> ( StringBackedStorage, Cmd StringBackedMsg )
        init_ _ = ( Dict.empty, Cmd.none )

        update_
            :  StringBackedMsg
            -> StringBackedStorage
            -> ( StringBackedStorage, Cmd StringBackedMsg )
        update_ (path, val) dict = ( dict |> Dict.insert path val, Cmd.none )

        view_ : StringBackedStorage -> Html StringBackedMsg
        view_ _ = Html.div [] []


        subscriptions_ : StringBackedStorage -> Sub StringBackedMsg
        subscriptions_ _ = Sub.none

    in
    element
        renderTarget
        (SendStrings
            { transmit = transmit >> Cmd.map (always ([], ""))
            }
        )
        { for = for_
        , init = init_
        , update = update_
        , view = view_
        , subscriptions = subscriptions_
        }


type alias ProxyBackedStorage = Dict ( Exp.RawPath, LabelPath ) ProxyValue


type alias ProxyBackedMsg = Maybe ( Exp.RawPath, LabelPath, ProxyValue )


{-| Program, backed with the proxy value storage. -}
type alias AppBackedByProxy flags model msg =
    ProgramWithTron flags ( ProxyBackedStorage, model ) ( ProxyBackedMsg, msg )


type alias BackedByProxy =
    AppBackedByProxy () () ()



type alias ValueAt = LabelPath -> Maybe ProxyValue


byProxy
    :  RenderTarget
    ->
        ( Exp.RawProperty -> Cmd msg
        , Exp.RawOutUpdate -> Cmd msg
        )
    -> (ValueAt -> Tron ())
    -> BackedByProxy
byProxy renderTarget ( ack, transmit ) for =
    byProxyApp
        renderTarget
        ( ack >> Cmd.map (always ())
        , transmit >> Cmd.map (always ())
        )
        { for = \valueAt _ -> for valueAt
        , init = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        , view = \_ _ -> Html.div [] []
        , update = \_ _ _ -> ( (), Cmd.none )
        }


byProxyApp
    :  RenderTarget
    ->
        ( Exp.RawProperty -> Cmd msg
        , Exp.RawOutUpdate -> Cmd msg
        )
    ->  { for : ValueAt -> model -> Tron msg
        , init : flags -> ValueAt -> ( model, Cmd msg )
        , subscriptions : ValueAt -> model -> Sub msg
        , view : ValueAt -> model -> Html msg
        , update : msg -> ValueAt -> model -> ( model, Cmd msg )
        }
    -> AppBackedByProxy flags model msg
byProxyApp renderTarget ( ack, transmit ) def =
    let

        valueAt dict =
            \path -> Dict.get path dict

        toValueAt : ProxyBackedStorage -> ValueAt
        toValueAt storage =
            storage
                |> Dict.mapKeys Tuple.second
                |> valueAt

        dictByPath : ProxyBackedStorage -> Dict Exp.RawPath ProxyValue
        dictByPath =
            Dict.mapKeys Tuple.first


        repair ( ( path, labelPath ), ( proxy, userMsg ) ) =
            ( Just ( Path.toList path, labelPath, proxy ), userMsg )

        for_ : ( ProxyBackedStorage, model ) -> Tron ( ProxyBackedMsg, msg )
        for_ ( dict, model ) =
            def.for (toValueAt dict) model
                |> Exp.toProxied
                |> Exp.loadProxyValues ( dictByPath dict )
                |> Property.addPaths
                |> Tron.map repair

        init_ : flags -> ( ( ProxyBackedStorage, model ), Cmd ( ProxyBackedMsg, msg ) )
        init_ flags =
            let
                storage = Dict.empty
                ( userModel, userEff ) =
                    def.init flags (toValueAt storage)
            in
                ( ( storage, userModel )
                , userEff
                    |> Cmd.map (Tuple.pair Nothing)
                )

        update_
            :  ( ProxyBackedMsg, msg )
            -> ( ProxyBackedStorage, model )
            -> ( ( ProxyBackedStorage, model ), Cmd ( ProxyBackedMsg, msg ) )
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

        view_ : ( ProxyBackedStorage, model ) -> Html ( ProxyBackedMsg, msg )
        view_ ( storage, model ) =
            def.view (toValueAt storage) model
                |> Html.map (Tuple.pair Nothing)


        subscriptions_ : ( ProxyBackedStorage, model ) -> Sub ( ProxyBackedMsg, msg )
        subscriptions_ ( storage, model ) =
            def.subscriptions (toValueAt storage) model
                |> Sub.map (Tuple.pair Nothing)

    in
    element
        renderTarget
        (SendJson
            { ack = ack >> Cmd.map (Tuple.pair Nothing)
            , transmit = transmit >> Cmd.map (Tuple.pair Nothing)
            }
        )
        { for = for_
        , init = init_
        , update = update_
        , view = view_
        , subscriptions = subscriptions_
        }

