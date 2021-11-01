module WithTron.Architecture exposing (..)


import Url exposing (Url)
import Task
import Html exposing (Html)
import Json.Decode as D

import Tron exposing (Tron)
import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Convert as Exp
import Tron.Tree.Expose.Tree as Exp
import Tron.Detach as Detach
import Tron.Core as Core exposing (State, Error)
import Tron.Tree.Internals as Tree
import Tron.Tree.Controls as Tree
import Tron.Tree.Values as Tree
import Tron.Tree.Paths as Tree
import Tron.Option.Render as  Render
import Tron.Option.Communication as Comm

import WithTron.Logic as L


type alias Tree = Tree.Tree ()


type WithTronMsg msg
    = ToUser msg
    | ToTron Core.Msg
    --| Ack Exp.Ack
    | SendUpdate Exp.Out
    | ReceiveRaw (List Exp.In)
    | ReplaceTree Tree
    | StoreError Error
    | SetClientId Detach.ClientId
    | UrlChange (Maybe msg) Url


init
    :  ( ( model, Cmd msg ), Tree -> model -> Tree )
    -> Maybe Url
    -> Render.Target
    -> Comm.Ports msg
    -> ( ( model, State, Tree ), Cmd (WithTronMsg msg) )
init ( userInit, userFor ) maybeUrl renderTarget ports =
    let
        ( initialModel, userEffect ) =
            userInit
        ( state, guiEffect ) =
            Core.init
        firstTree =
            userFor Tree.empty initialModel
    in
        (
            ( initialModel
            , state |> L.addInitOptions renderTarget
            , firstTree
            )
        , Cmd.batch
            [ userEffect |> Cmd.map ToUser
            , guiEffect |> Cmd.map ToTron
            , L.performInitEffects -- FIXME: `clientId` is always `Nothing`
                (state.detach |> Tuple.first)
                ports
                firstTree
                |> Cmd.map ToUser
            , case maybeUrl of
                Just url ->
                    Task.succeed url
                        |> Task.perform (UrlChange Nothing)
                Nothing -> Cmd.none
            ]
        )


view
    :  ( Tree -> model -> Html msg )
    -> Render.Target
    -> ( model, State, Tree )
    -> Html (WithTronMsg msg)
view userView renderTarget ( model, state, tree ) =
    Html.div
        [ ]
        [ tree
            |> L.useRenderTarget renderTarget state
            |> Html.map ToTron
        , userView tree model
            |> Html.map ToUser
        ]


subscriptions
    :  ( Tree -> model -> Sub msg )
    -> Comm.Ports msg
    -> ( model, State, Tree )
    -> Sub (WithTronMsg msg)
subscriptions userSubscriptions ports ( model, state, tree ) =
    let
        convertIncoming incoming =
            case incoming of
                L.FullTree decodeResult ->
                    case decodeResult of
                        Ok newTree -> ReplaceTree newTree
                        Err decodeError -> StoreError <| D.errorToString decodeError
                L.Updates updates -> ReceiveRaw updates

    in Sub.batch
        [ userSubscriptions tree model |> Sub.map ToUser
        , Core.subscriptions state |> Sub.map ToTron
        , L.addSubscriptionsOptions ports tree |> Sub.map convertIncoming
        ]


update
    : ( msg -> Tree -> model -> (model, Cmd msg)
      , Tree -> model -> Tron msg
      )
    -> Comm.Ports msg
    -> WithTronMsg msg
    -> ( model, State, Tree )
    -> ( ( model, State, Tree ), Cmd (WithTronMsg msg) )
update ( userUpdate, userFor ) ports withTronMsg ( model, state, prevTree ) =
    case withTronMsg of

        ToUser userMsg ->
            let
                ( newUserModel, userEffect ) =
                    userUpdate userMsg prevTree model
                nextTree =
                    userFor prevTree newUserModel
            in

            (
                ( newUserModel
                , state
                , nextTree
                    |> Tree.toUnit
                    |> Tree.transferTransientState prevTree
                    |> Tree.loadValues prevTree
                    --|> Tree.loadChangedValues prevTree nextTree
                    --|> Tree.loadLiveValues nextGui
                )
            , userEffect |> Cmd.map ToUser
            )

        ToTron guiMsg ->
            let
                tree = userFor prevTree model
                unitTree = tree |> Tree.toUnit
            in case {- prevTree
                    |> Exp.lift -}
                Core.update guiMsg state
                    <| (
                            unitTree
                                |> Tree.transferTransientState prevTree
                                |> Tree.loadValues prevTree
                                |> Tree.squeezeMap2 (\handler _ -> handler) tree
                       --|> Tree.loadLiveValues nextGui
                       )  of
                ( nextState, nextTree, guiEffect ) ->
                    (
                        ( model
                        , nextState
                        , nextTree
                        )
                    , Cmd.batch
                        [ guiEffect
                            |> Cmd.map (Tuple.second >> ToUser)
                        , guiEffect
                            |> Cmd.map (Tuple.first >> SendUpdate)
                        ]
                    )

        ReceiveRaw rawUpdates ->
            let
                tree = userFor prevTree model
                unitRoot =
                    tree
                        |> Tree.toUnit
                        |> Tree.transferTransientState prevTree
                nextRoot =
                    rawUpdates
                        |> List.foldl (Exp.apply << Exp.fromPort) unitRoot
                        --|> Exp.apply (Exp.fromPort rawUpdate)
            in
                (
                    ( model
                    , state
                    , nextRoot
                    )
                , nextRoot
                    |> Tree.squeezeMap2 (\handler _ -> handler) tree
                    |> Tron.perform
                    |> Cmd.map ToUser
                )

        SetClientId clientId ->
            (
                ( model
                ,
                    { state
                    | detach =
                        case state.detach of
                            ( _, detachState ) -> ( Just clientId, detachState )

                    }
                , prevTree
                )
            , case ports.ack of -- FIXME: move to WithTron.Logic
                Just ack ->
                    Exp.makeAck
                        (Detach.clientIdToString clientId)
                        prevTree
                        |> ack
                        |> Cmd.map ToUser
                _ -> Cmd.none
            )

        SendUpdate rawUpdate ->
            ( ( model, state, prevTree )
            , rawUpdate
                |> L.tryTransmitting ports
                |> Cmd.map ToUser
            )

        UrlChange maybeUserMsg url ->
            let
                detachState = Detach.fromUrl url
                urlEffects = applyUrl ports prevTree url
            in
                (
                    ( model
                    ,
                        { state
                        | detach = detachState
                        }
                    , prevTree
                    )
                , Cmd.batch
                    [ urlEffects
                    , case maybeUserMsg of
                        Just userMsg ->
                            Task.succeed userMsg
                                |> Task.perform identity
                                |> Cmd.map ToUser
                        Nothing -> Cmd.none
                    ]
                )

        ReplaceTree nextTree ->
            (
                ( model
                , state -- TODO : clear `detach` & `mouse`
                    -- { state | mouse = Tron.Mouse.init }
                , nextTree
                )
            , Cmd.none
            )

        StoreError error ->
            ( ( model, { state | errors = error :: state.errors }, prevTree )
            , Cmd.none
            )


-- FIXME: move to WithTron.Logic
applyUrl
    :  Comm.Ports msg
    -> Tree
    -> Url.Url
    -> Cmd (WithTronMsg msg)
applyUrl ports tree url =
    let
        ( maybeClientId, state ) = Detach.fromUrl url
    in
        case ( ports.ack, maybeClientId ) of
            ( Just ack, Just clientId ) ->
                Exp.makeAck
                    (Detach.clientIdToString clientId)
                    tree
                    |> ack
                    |> Cmd.map ToUser
                -- Task.succeed id
                --     |> Task.perform SetClientId
            ( Just ack, Nothing ) ->
                L.nextClientId
                    |> Cmd.map SetClientId

            _ -> Cmd.none
