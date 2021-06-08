module WithTron.Logic exposing (..)

import Random

import Tron exposing (Tron)
import Tron.Expose as Exp
import Tron.Expose.Convert as Exp
import Tron.Expose.Data as Exp
import Tron.Option exposing (..)
import Tron.Core as Core exposing (State)
import Tron.Detach as Detach
import Tron.Render.Layout as L

import Html exposing (Html)


performInitEffects : PortCommunication msg -> Tron () -> Cmd msg
performInitEffects ports tree =
    case ports of
        SendJson { ack } ->
            tree |> Exp.encode |> ack
        SendReceiveJson { ack } ->
            tree |> Exp.encode |> ack
        DatGui { ack } ->
            tree |> Exp.encode |> ack
        _ -> Cmd.none


tryTransmitting : PortCommunication msg -> Exp.Out -> Cmd msg
tryTransmitting ports rawUpdate =
    case ports of
        Detachable { transmit } ->
            transmit rawUpdate
        SendJson { transmit } ->
            transmit rawUpdate
        SendReceiveJson { transmit } ->
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
        Debug dock _ -> gui |> Core.dock dock


addSubscriptionsOptions : PortCommunication msg -> Tron () -> Sub (List Exp.In)
addSubscriptionsOptions ports tree =
    case ports of
        SendReceiveJson { apply } ->
            apply
                |> Sub.map (List.map <| Core.tryDeduce tree)
                |> Sub.map (List.filterMap identity)
        Detachable { receive } ->
            receive |> Sub.map List.singleton
        DatGui { receive } ->
            receive |> Sub.map List.singleton
        _ -> Sub.none


useRenderTarget : RenderTarget -> State -> Tron () -> Html Core.Msg
useRenderTarget target state tree =
    case target of
        Html dock theme -> tree |> Core.view L.Fancy theme (state |> Core.dock dock)
        Nowhere -> Html.div [] []
        Aframe _ -> Html.div [] [] -- FIXME
        Debug dock theme -> tree |> Core.view L.Debug theme (state |> Core.dock dock)


setDetachState : ( Maybe Detach.ClientId, Detach.State ) -> State -> State
setDetachState detachState state =
    { state
    | detach = detachState
    }


nextClientId : Cmd Detach.ClientId
nextClientId =
    Random.generate identity Detach.clientIdGenerator