module Gui.WithGui exposing (..)


import Browser
import Html exposing (Html)
--import Either exposing (Either(..))
import Random

import Gui as Tron
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Option exposing (..)
import Gui.Msg exposing (Msg_(..))
import Gui.Detach as Detach


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
            |> addViewOptions options
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
                    |> Tron.over (userFor model)
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
                , nextRoot
                    |> Exp.update (Exp.fromPort rawUpdate)
                    |> Cmd.map ToUser
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


        SendUpdate _ ->
            ( (model, gui)
            , Cmd.none
            ) -- FIXME:


addInitOptions : List (Option msg) -> Tron.Gui msg -> Tron.Gui msg
addInitOptions options gui =
    options
        |> List.foldl
            (\option gui_ ->
                case option of
                    Dock target ->
                        gui_ |> Tron.dock target
                    _ ->
                        gui_
            )
            gui


performInitEffects : List (Option msg) -> Tron.Gui msg -> Cmd msg
performInitEffects options gui =
    options
        |> List.foldl
            (\option cmds ->
                case option of
                    SendJsonToJs { ack } ->
                        (gui
                            |> Tron.encode
                            |> ack
                        ) :: cmds
                    _ ->
                        cmds
            )
            []
        |> Cmd.batch



-- FIXME: Use in WithGui at `init`
{- detachable
     : Url
    -> (Exp.Ack -> Cmd msg)
    -> Gui msg
    -> ( Gui msg, Cmd Msg )
detachable url ack gui =
    let
        ( maybeClient, state ) = Detach.fromUrl url
    in
        (
            { gui
            | detach = ( maybeClient, state )
            }
        , case maybeClient of
            Nothing -> Detach.nextClientId
            _ ->
                Exp.encodeAck maybeClient
                    |> ack
                    |> Cmd.map (always NoOp)
        ) -}


{-
performUpdateEffects : List (Option msg) -> Tron.Gui ( Exp.RawUpdate, msg ) -> Cmd (WithGuiMsg msg)
performUpdateEffects options gui =
    options
        |> List.foldl
            (\option cmds ->
                case option of
                    SendJsonToJs { transmit } ->
                        (gui
                            |> Tron.toExposed
                            |> Tron.map Tuple.second -- FIXME: perform the update before
                            |> Tron.over gui.tree
                            |> Tron.update msg -- FIXME: this way, we call the update twice
                            |> Tuple.second
                            |> Cmd.map SendUpdate
                            --|> Cmd.andThen transmit
                        ) :: cmds
                    _ ->
                        cmds
            )
            []
        |> Cmd.batch -}


addSubscriptionsOptions : List (Option msg) -> Tron.Gui msg -> Sub msg
addSubscriptionsOptions options gui =
    Sub.none -- FIXME:


addViewOptions : List (Option msg) -> Tron.Gui msg -> Html Tron.Msg
addViewOptions options gui =
    options
        |> List.foldl
            (\option _ ->
                case option of
                    Hidden ->
                        []
                    Theme theme ->
                        [ gui |> Tron.view theme ]
                    -- FIXME: AFrame
                    _ ->
                        [ gui |> Tron.view Theme.light ]
            )
            []
        |> Html.div []


nextClientId : Cmd (WithGuiMsg msg)
nextClientId =
    Random.generate SetClientId Detach.clientIdGenerator


element
    :
        { options : List (Option msg)
        , for : model -> Builder.Builder msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> ProgramWithGui flags model msg
element def =
    Browser.element
        { init =
            init ( def.init, def.for ) def.options
        , view =
            view def.view def.options
        , subscriptions =
            subscriptions def.subscriptions def.options
        , update =
            update ( def.update, def.for ) def.options
        }


