module WithTron.Logic exposing (..)

import Random

import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Json as Exp
import Tron.Option.Communication exposing (Ports)
import Tron.Option.Render as Render exposing (Target(..))
import Tron.Core as Core exposing (State)
import Tron.Detach as Detach
import Tron.Render.Layout as L
import Tron.Tree exposing (Tree)
import HashId

import Json.Encode as E

import Html exposing (Html)


{- setDetachState : ( Maybe Detach.ClientId, Detach.State ) -> State -> State
setDetachState detachState state =
    { state
    | detach = detachState
    } -}


nextClientId : Cmd Detach.ClientId
nextClientId =
    Random.generate identity Detach.clientIdGenerator


performInitEffects : Maybe Detach.ClientId -> Ports msg -> Tree () -> Cmd msg
performInitEffects maybeClientId { ack } tree =
    ack
        |> Maybe.map
            ((|>)
                { client =
                    maybeClientId
                            |> Maybe.map (HashId.toString >> E.string)
                            |> Maybe.withDefault E.null
                , tree = Exp.encode <| tree
                }
            )
        |> Maybe.withDefault Cmd.none


tryTransmitting : Ports msg -> Exp.Out -> Cmd msg
tryTransmitting { transmit } rawUpdate =
    transmit
        |> Maybe.map ((|>) rawUpdate)
        |> Maybe.withDefault Cmd.none


addSubscriptionsOptions : Ports msg -> Tree () -> Sub (List Exp.In)
addSubscriptionsOptions { apply, receive } tree =
    Sub.batch
        [ apply
            |> Maybe.map (Sub.map (List.map <| Core.tryDeduce tree))
            |> Maybe.map (Sub.map (List.filterMap identity))
            |> Maybe.withDefault Sub.none
        , receive
            |> Maybe.map (Sub.map List.singleton)
            |> Maybe.withDefault Sub.none
        ]


addInitOptions : Render.Target -> State -> State
addInitOptions target gui =
    case target of
        Html dock _ -> gui |> Core.dock dock
        Nowhere -> gui
        Aframe _ -> gui
        Debug dock _ -> gui |> Core.dock dock


useRenderTarget : Render.Target -> State -> Tree () -> Html Core.Msg
useRenderTarget target state tree =
    case target of
        Html dock theme -> tree |> Core.view L.Fancy theme (state |> Core.dock dock)
        Nowhere -> Html.div [] []
        Aframe _ -> Html.div [] [] -- FIXME
        Debug dock theme -> tree |> Core.view L.Debug theme (state |> Core.dock dock)