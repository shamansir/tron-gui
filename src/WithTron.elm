module WithTron exposing (..)


import Browser exposing (UrlRequest(..))
import Url exposing (Url)
import Html exposing (Html)
--import Either exposing (Either(..))
import Random
import Dict exposing (Dict)
import Task

import Tron exposing (Tron)
import Tron.Builder as Builder exposing (Builder)
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock exposing (Dock(..))
import Tron.Expose as Exp
import Tron.Option exposing (..)
import Tron.Msg exposing (Msg_(..))
import Tron.Detach as Detach
import Tron.Property as Property exposing (LabelPath)


type alias ProgramWithTron flags model msg =
    Program flags ( model, Tron msg ) (WithTronMsg msg)


type WithTronMsg msg
    = ToUser msg
    | ToTron Tron.Msg
    --| Ack Exp.Ack
    | SendUpdate Exp.RawOutUpdate
    | ReceiveRaw Exp.RawInUpdate
    | SetClientId Detach.ClientId
    | UrlChange (Maybe msg) Url


init
    :  ( flags -> ( model, Cmd msg ), model -> Builder msg )
    -> Maybe Url
    -> RenderTarget
    -> PortCommunication msg
    -> flags
    -> ( ( model, Tron msg ), Cmd (WithTronMsg msg) )
init ( userInit, userFor ) maybeUrl renderTarget ports flags =
    let
        ( initialModel, userEffect ) =
            userInit flags
        ( gui, guiEffect ) =
            userFor initialModel
                |> Tron.init
    in
        (
            ( initialModel
            , gui
                |> addInitOptions renderTarget
            )
        , Cmd.batch
            [ userEffect |> Cmd.map ToUser
            , guiEffect |> Cmd.map ToTron
            , performInitEffects ports gui |> Cmd.map ToUser
            , case maybeUrl of
                Just url -> applyUrl ports url
                Nothing -> Cmd.none
            ]
        )


view
    :  (model -> Html msg)
    -> RenderTarget
    -> (model, Tron msg)
    -> Html (WithTronMsg msg)
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
    -> ( model, Tron msg )
    -> Sub (WithTronMsg msg)
subscriptions userSubscriptions ports ( model, gui ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map ToUser
        , Tron.subscriptions gui |> Sub.map ToTron
        , addSubscriptionsOptions ports gui |> Sub.map ReceiveRaw
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Builder msg )
    -> PortCommunication msg
    -> WithTronMsg msg
    -> ( model, Tron msg )
    -> ( ( model, Tron msg ), Cmd (WithTronMsg msg) )
update ( userUpdate, userFor ) ports withTronMsg (model, gui) =
    case withTronMsg of

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
            )

        SendUpdate rawUpdate ->
            ( ( model, gui )
            , rawUpdate
                |> tryTransmitting ports
                |> Cmd.map ToUser
            )

        UrlChange maybeUserMsg url ->
            let
                detachState = Detach.fromUrl url
                urlEffects = applyUrl ports url
            in
                (
                    ( model
                    ,
                        { gui
                        | detach = detachState
                        }
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


performInitEffects : PortCommunication msg -> Tron msg -> Cmd msg
performInitEffects ports gui =
        case ports of
            SendJson { ack } ->
                gui |> Tron.encode |> ack
            DatGui { ack } ->
                gui |> Tron.encode |> ack
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
                ( rawUpdate.labelPath |> String.join "/"
                , rawUpdate.stringValue
                )
        _ -> Cmd.none


addInitOptions : RenderTarget -> Tron msg -> Tron msg
addInitOptions target gui =
    case target of
        Html dock _ -> gui |> Tron.dock dock
        Nowhere -> gui
        Aframe _ -> gui


addSubscriptionsOptions : PortCommunication msg -> Tron msg -> Sub Exp.RawInUpdate
addSubscriptionsOptions ports gui =
    case ports of
        DatGui { receive } ->
            receive
        _ -> Sub.none


useRenderTarget : RenderTarget -> Tron msg -> Html Tron.Msg
useRenderTarget target gui =
    case target of
        Html dock theme -> gui |> Tron.dock dock |> Tron.view theme
        Nowhere -> Html.div [] []
        Aframe _ -> Html.div [] [] -- FIXME


setDetachState : ( Maybe Detach.ClientId, Detach.State ) -> Tron msg -> Tron msg
setDetachState st gui =
    { gui
    | detach = st
    }


applyUrl
    :  PortCommunication msg
    -> Url.Url
    -> Cmd (WithTronMsg msg)
applyUrl ports url =
    let
        ( maybeClientId, state ) = Detach.fromUrl url
    in
        case ( ports, maybeClientId ) of
            ( Detachable { ack }, Just clientId ) ->
                Exp.encodeAck clientId
                    |> ack
                    |> Cmd.map ToUser
                -- Task.succeed id
                --     |> Task.perform SetClientId
            ( Detachable { ack }, Nothing ) ->
                nextClientId
            _ -> Cmd.none


nextClientId : Cmd (WithTronMsg msg)
nextClientId =
    Random.generate SetClientId Detach.clientIdGenerator


sandbox
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Builder.Builder msg
        , init : model
        , view : model -> Html msg
        , update : msg -> model -> model
        }
    -> ProgramWithTron flags model msg
sandbox renderTarget ports impl =
    element
        renderTarget
        ports
        { for = impl.for
        , init = \_ -> ( impl.init, Cmd.none )
        , view = impl.view
        , update = \msg model -> ( impl.update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


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
    -> ProgramWithTron flags model msg
element renderTarget ports def =
    Browser.element
        { init =
            init ( def.init, def.for ) Nothing renderTarget ports
        , update =
            update ( def.update, def.for ) ports
        , view =
            view def.view renderTarget
        , subscriptions =
            subscriptions def.subscriptions ports
        }


document
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Builder.Builder msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> ProgramWithTron flags model msg
document renderTarget ports def =
    Browser.document
        { init =
            \flags ->
                init ( def.init, def.for ) Nothing renderTarget ports flags
        , update =
            update ( def.update, def.for ) ports
        , view =
            \(userModel, gui) ->
                { title = (def.view userModel).title
                , body =
                    [ view
                        (\umodel ->
                            -- FIXME: we calculate view two times, it seems
                            Html.div [] <| (def.view umodel).body
                        )
                        renderTarget
                        (userModel, gui)
                    ]
                }
        , subscriptions =
            subscriptions def.subscriptions ports
        }


application
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Builder.Builder msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , onUrlChange : Url.Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        }
    -> ProgramWithTron flags model msg
application renderTarget ports def =
    Browser.application
        { init =
            \flags url _ ->
                init ( def.init, def.for ) (Just url) renderTarget ports flags
        , update =
            update ( def.update, def.for ) ports
        , view =
            \(userModel, gui) ->
                let userView = def.view userModel
                in
                    { title = userView.title
                    , body =
                        [ view
                            (\umodel ->
                                -- FIXME: we calculate view two times, it seems
                                Html.div [] <| userView.body
                            )
                            renderTarget
                            (userModel, gui)
                        ]
                    }
        , subscriptions =
            subscriptions def.subscriptions ports
        , onUrlChange =
            \url -> UrlChange (Just <| def.onUrlChange url) url
        , onUrlRequest =
            \req ->
                case req of
                    Internal url ->
                        UrlChange (Just <| def.onUrlRequest req) <| url
                    External _ ->
                        ToUser <| def.onUrlRequest req
        }


type alias BackedStorage = Dict LabelPath String


type alias BackedMsg = ( LabelPath, String ) -- message is just key & value to put in the dict


type alias BackedWithTron = ProgramWithTron () BackedStorage BackedMsg


backed
    :  RenderTarget
    -> (( String, String ) -> Cmd msg)
    -> Builder ()
    -> BackedWithTron
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
