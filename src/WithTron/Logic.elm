module WithTron.Logic exposing (..)

import Random

import Tron exposing (Tron)
import Tron.Expose as Exp
import Tron.Expose.Convert as Exp
import Tron.Expose.Data as Exp
import Tron.Option exposing (..)
import Tron.Core as Core exposing (State)
import Tron.Detach as Detach

import Html exposing (Html)


performInitEffects : PortCommunication msg -> Tron () -> Cmd msg
performInitEffects ports tree =
        case ports of
            SendJson { ack } ->
                tree |> Exp.encode |> ack
            DatGui { ack } ->
                tree |> Exp.encode |> ack
            _ -> Cmd.none


tryTransmitting : PortCommunication msg -> Exp.RawOutUpdate -> Cmd msg
tryTransmitting ports rawUpdate =
    case ports of
        Detachable { transmit } ->
            transmit rawUpdate
        SendJson { transmit } ->
            transmit rawUpdate
        SendStrings { transmit } ->
            transmit
                ( rawUpdate.update.labelPath
                , rawUpdate.update.stringValue
                )
        _ -> Cmd.none


addInitOptions : RenderTarget -> State -> State
addInitOptions target gui =
    case target of
        Html dock _ -> gui |> Core.dock dock
        Nowhere -> gui
        Aframe _ -> gui


addSubscriptionsOptions : PortCommunication msg -> Sub Exp.RawInUpdate
addSubscriptionsOptions ports =
    case ports of
        Detachable { receive } ->
            receive
        DatGui { receive } ->
            receive
        _ -> Sub.none


useRenderTarget : RenderTarget -> State -> Tron () -> Html Core.Msg
useRenderTarget target state tree =
    case target of
        Html dock theme -> tree |> Core.view theme (state |> Core.dock dock)
        Nowhere -> Html.div [] []
        Aframe _ -> Html.div [] [] -- FIXME


setDetachState : ( Maybe Detach.ClientId, Detach.State ) -> State -> State
setDetachState detachState state =
    { state
    | detach = detachState
    }


nextClientId : Cmd Detach.ClientId
nextClientId =
    Random.generate identity Detach.clientIdGenerator