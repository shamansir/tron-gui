module Gui.WithGui exposing (..)


import Browser
import Html exposing (Html)
--import Either exposing (Either(..))
import Random
import Dict exposing (Dict)

import Gui as Tron
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Option exposing (..)
import Gui.Msg exposing (Msg_(..))
import Gui.Detach as Detach
import Gui.Property as Property exposing (LabelPath)


type alias ProgramWithGui flags model msg =
    Program flags ( model, Tron.Gui msg ) (WithGuiMsg msg)


type WithGuiMsg msg
    = ToUser msg
    | ToTron Tron.Msg
    --| Ack Exp.Ack
    | SendUpdate Exp.RawUpdate
    | ReceiveRaw Exp.RawUpdate
    | SetClientId Detach.ClientId


init
    :  ( flags -> ( model, Cmd msg ), model -> Builder msg )
    -> List (Option msg)
    -> flags
    -> ( ( model, Tron.Gui msg ), Cmd (WithGuiMsg msg) )
init ( userInit, userFor ) options flags =
    let
        ( initialModel, userEffect ) =
            userInit flags
        ( gui, guiEffect ) =
            userFor initialModel
                |> Tron.init

    in
        (
            ( initialModel
            , gui |> addInitOptions options
            )
        , Cmd.batch
            [ userEffect |> Cmd.map ToUser
            , guiEffect |> Cmd.map ToTron
            , performInitEffects options gui |> Cmd.map ToUser
            ]
        )


view
    :  (model -> Html msg)
    -> List (Option msg)
    -> (model, Tron.Gui msg)
    -> Html (WithGuiMsg msg)
view userView options ( model, gui ) =
    Html.div
        [ ]
        [ gui
            |> useRenderTarget options
            |> Html.map ToTron
        , userView model
            |> Html.map ToUser
        ]


subscriptions
    :  ( model -> Sub msg )
    -> List (Option msg)
    -> ( model, Tron.Gui msg )
    -> Sub (WithGuiMsg msg)
subscriptions userSubscriptions options ( model, gui ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map ToUser
        , Tron.subscriptions gui |> Sub.map ToTron
        , addSubscriptionsOptions options gui |> Sub.map ToUser
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Builder msg )
    -> List (Option msg)
    -> WithGuiMsg msg
    -> ( model, Tron.Gui msg )
    -> ( ( model, Tron.Gui msg ), Cmd (WithGuiMsg msg) )
update ( userUpdate, userFor ) options withGuiMsg (model, gui) =
    case withGuiMsg of

        ToUser userMsg ->
            let
                ( newUserModel, userEffect ) =
                    userUpdate userMsg model
            in

            (
                ( newUserModel
                , gui
                    |> Tron.over (userFor newUserModel)
                )
            , userEffect |> Cmd.map ToUser
            )

        ToTron guiMsg ->
            case gui |> Tron.toExposed |> Tron.update guiMsg of
                ( nextGui, guiEffect ) ->
                    (
                        ( model
                        , nextGui |> Tron.map Tuple.second
                        )
                    , Cmd.batch
                        [ guiEffect
                            |> Cmd.map (Tuple.second >> ToUser)
                        , guiEffect
                            |> Cmd.map (Tuple.first >> SendUpdate)
                        ]
                    )

        ReceiveRaw rawUpdate ->
            let
                nextRoot =
                    gui.tree
                        |> Exp.apply (Exp.fromPort rawUpdate)
            in
                (
                    ( model
                    ,
                        { gui
                        | tree = nextRoot
                        }
                    )
                , Cmd.none {- nextRoot
                    |> Exp.update (Exp.fromPort rawUpdate)
                    |> Cmd.map ToUser -}
                )

        SetClientId clientId ->
            (
                ( model
                ,
                    { gui
                    | detach =
                        case gui.detach of
                            ( _, state ) -> ( Just clientId, state )

                    }
                )
            , Cmd.none
            -- , nextDetach |> Detach.ack -- FIXME:
            )

        SendUpdate rawUpdate ->
            ( ( model, gui )
            , rawUpdate
                |> tryTransmitting options
                |> Cmd.map ToUser
            )



performInitEffects : List (Option msg) -> Tron.Gui msg -> Cmd msg
performInitEffects options gui =
    case getCommunication options of
        SendJson { ack } ->
            gui
                |> Tron.encode
                |> ack
        _ -> Cmd.none


tryTransmitting : List (Option msg) -> Exp.RawUpdate -> Cmd msg
tryTransmitting options rawUpdate =
    case getCommunication options of
        SendJson { transmit } ->
            transmit rawUpdate
        SendStrings { transmit } ->
            transmit
                ( rawUpdate.labelPath |> String.join "/"
                , rawUpdate.stringValue
                )
        _ -> Cmd.none


addInitOptions : List (Option msg) -> Tron.Gui msg -> Tron.Gui msg
addInitOptions options gui =
    case getRenderTarget options of
        Html dock _ -> gui |> Tron.dock dock
        Nowhere -> gui
        Aframe -> gui


addSubscriptionsOptions : List (Option msg) -> Tron.Gui msg -> Sub msg
addSubscriptionsOptions options gui =
    Sub.none -- FIXME:


useRenderTarget : List (Option msg) -> Tron.Gui msg -> Html Tron.Msg
useRenderTarget options gui =
    case getRenderTarget options of
        Html dock theme -> gui |> Tron.dock dock |> Tron.view theme
        Nowhere -> Html.div [] []
        Aframe -> Html.div [] [] -- FIXME


nextClientId : Cmd (WithGuiMsg msg)
nextClientId =
    Random.generate SetClientId Detach.clientIdGenerator


element
    :   List (Option msg)
    ->
        { for : model -> Builder.Builder msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> ProgramWithGui flags model msg
element options def =
    Browser.element
        { init =
            init ( def.init, def.for ) options
        , view =
            view def.view options
        , subscriptions =
            subscriptions def.subscriptions options
        , update =
            update ( def.update, def.for ) options
        }


stored
    :  List (Option ( LabelPath, String ))
    -> Builder ()
    -> ProgramWithGui () (Dict LabelPath String) ( LabelPath, String )
stored options tree =
    let

        tree_ : Builder ( LabelPath, String )
        tree_ = tree |> Exp.toStrExposed |> Property.map Tuple.first

        for_ : Dict LabelPath String -> Builder ( LabelPath, String )
        for_ dict = tree_ |> Exp.loadValues dict

        init_ : () -> ( Dict LabelPath String, Cmd ( LabelPath, String ) )
        init_ _ = ( Dict.empty, Cmd.none )

        update_
            :  ( LabelPath, String )
            -> Dict LabelPath String
            -> ( Dict LabelPath String, Cmd ( LabelPath, String ) )
        update_ (path, val) dict = ( dict |> Dict.insert path val, Cmd.none )

        view_ : Dict LabelPath String -> Html ( LabelPath, String )
        view_ _ = Html.div [] []


        subscriptions_ : Dict LabelPath String -> Sub ( LabelPath, String )
        subscriptions_ _ = Sub.none

    in
    element
        options
        { for = for_
        , init = init_
        , update = update_
        , view = view_
        , subscriptions = subscriptions_
        }
