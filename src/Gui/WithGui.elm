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
    -> RenderTarget
    -> PortCommunication msg
    -> flags
    -> ( ( model, Tron.Gui msg ), Cmd (WithGuiMsg msg) )
init ( userInit, userFor ) renderTarget ports flags =
    let
        ( initialModel, userEffect ) =
            userInit flags
        ( gui, guiEffect ) =
            userFor initialModel
                |> Tron.init

    in
        (
            ( initialModel
            , gui |> addInitOptions renderTarget
            )
        , Cmd.batch
            [ userEffect |> Cmd.map ToUser
            , guiEffect |> Cmd.map ToTron
            , performInitEffects ports gui |> Cmd.map ToUser
            ]
        )


view
    :  (model -> Html msg)
    -> RenderTarget
    -> (model, Tron.Gui msg)
    -> Html (WithGuiMsg msg)
view userView renderTarget ( model, gui ) =
    Html.div
        [ ]
        [ gui
            |> useRenderTarget renderTarget
            |> Html.map ToTron
        , userView model
            |> Html.map ToUser
        ]


subscriptions
    :  ( model -> Sub msg )
    -> PortCommunication msg
    -> ( model, Tron.Gui msg )
    -> Sub (WithGuiMsg msg)
subscriptions userSubscriptions ports ( model, gui ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map ToUser
        , Tron.subscriptions gui |> Sub.map ToTron
        , addSubscriptionsOptions ports gui |> Sub.map ToUser
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Builder msg )
    -> PortCommunication msg
    -> WithGuiMsg msg
    -> ( model, Tron.Gui msg )
    -> ( ( model, Tron.Gui msg ), Cmd (WithGuiMsg msg) )
update ( userUpdate, userFor ) ports withGuiMsg (model, gui) =
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
                |> tryTransmitting ports
                |> Cmd.map ToUser
            )



performInitEffects : PortCommunication msg -> Tron.Gui msg -> Cmd msg
performInitEffects ports gui =
    case ports of
        SendJson { ack } ->
            gui
                |> Tron.encode
                |> ack
        _ -> Cmd.none


tryTransmitting : PortCommunication msg -> Exp.RawUpdate -> Cmd msg
tryTransmitting ports rawUpdate =
    case ports of
        SendJson { transmit } ->
            transmit rawUpdate
        SendStrings { transmit } ->
            transmit
                ( rawUpdate.labelPath |> String.join "/"
                , rawUpdate.stringValue
                )
        _ -> Cmd.none


addInitOptions : RenderTarget -> Tron.Gui msg -> Tron.Gui msg
addInitOptions target gui =
    case target of
        Html dock _ -> gui |> Tron.dock dock
        Nowhere -> gui
        Aframe _ -> gui


addSubscriptionsOptions : PortCommunication msg -> Tron.Gui msg -> Sub msg
addSubscriptionsOptions ports gui =
    Sub.none -- FIXME:


useRenderTarget : RenderTarget -> Tron.Gui msg -> Html Tron.Msg
useRenderTarget target gui =
    case target of
        Html dock theme -> gui |> Tron.dock dock |> Tron.view theme
        Nowhere -> Html.div [] []
        Aframe _ -> Html.div [] [] -- FIXME


nextClientId : Cmd (WithGuiMsg msg)
nextClientId =
    Random.generate SetClientId Detach.clientIdGenerator


element
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Builder.Builder msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> ProgramWithGui flags model msg
element renderTarget ports def =
    Browser.element
        { init =
            init ( def.init, def.for ) renderTarget ports
        , view =
            view def.view renderTarget
        , subscriptions =
            subscriptions def.subscriptions ports
        , update =
            update ( def.update, def.for ) ports
        }


type alias BackedStorage = Dict LabelPath String


type alias BackedMsg = ( LabelPath, String ) -- message is just key & value to put in the dict


type alias BackedWithGui = ProgramWithGui () BackedStorage BackedMsg


backed
    :  RenderTarget
    -> (( String, String ) -> Cmd msg)
    -> Builder ()
    -> BackedWithGui
backed renderTarget transmit tree =
    let

        tree_ : Builder BackedMsg
        tree_ = tree |> Exp.toStrExposed |> Property.map Tuple.first

        for_ : BackedStorage -> Builder BackedMsg
        for_ dict = tree_ |> Exp.loadValues dict

        init_ : () -> ( BackedStorage, Cmd BackedMsg )
        init_ _ = ( Dict.empty, Cmd.none )

        update_
            :  BackedMsg
            -> BackedStorage
            -> ( BackedStorage, Cmd BackedMsg )
        update_ (path, val) dict = ( dict |> Dict.insert path val, Cmd.none )

        view_ : BackedStorage -> Html BackedMsg
        view_ _ = Html.div [] []


        subscriptions_ : BackedStorage -> Sub BackedMsg
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
