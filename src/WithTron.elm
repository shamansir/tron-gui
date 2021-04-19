module WithTron exposing
    ( ProgramWithTron
    , sandbox, element, document, application
    , backed, BackedStorage, BackedWithTron
    )


{-| `WithTron` is the set of functions which wrap Elm `Browser.*` helpers and so
let you easily add your GUI to the usual Elm-driven flow.

Here is the `OneKnob` example:

The only things that is different from usual `Elm` application is `for` function which allows you to build the Tron GUI using current state of the model:

    import WithTron exposing (ProgramWithTron)

    type alias Amount = Float

    type Msg
        = AmountChanged Amount

    type alias Model = Amount

    for : Model -> Tron Msg
    for amount =
        Builder.root
            [
                ( "amount"
                , Builder.float
                    { min = 0, max = 1, step = 0.01 }
                    amount
                    AmountChanged
                )
            ]

    init _ =
        ( 0, Cmd.none )

    view amount =
        Html.text
            <| String.fromFloat amount

    update msg curAmount =
        case msg of

            AmountChanged newAmount ->
                ( newAmount
                , Cmd.none
                )

    subscriptions _ = Sub.none

    main : ProgramWithTron () Model Msg
    main =
        WithTron.element
            (Option.toHtml Dock.center Theme.dark)
            Option.noCommunication
            { for = for
            , init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

For help on how to define your interface with `for` function, see the detailed `Tron.Builder` documentation.

Sometimes  you don't even need the `init`/`view`/`update`, for example to connect GUI to the JavaScript or something remote. In this case the `backed` function lets you define the application which just stores the path-to-value map and nothing else.

More examples are in the `README`, and in the `example/*/Main.elm` modules.

See also: `Tron.Option`, `Tron.Builder`, `Tron.Builder.*`.

There are some special types of GUI Builders that can come in handy if you don't want to use messages, but get them as the type-value pairs or add paths to them or just skip them. All of them doesn't require you to specify handling message so, every function from such `Builder` has one argument less:

- `Tron ()` (`Tron.Builder.Unit`) — do not expose values (keeps them inside);
- `Tron ProxyValue` (`Tron.Builder.Proxy`) — store values as a type-value data, see `Tron.Expose.ProxyValue` for more info;
- `Tron String` (`Tron.Builder.String`) — store value stringified;

Any `Tron a` can be converted to `Tron ( Path, Value )` using `Builder.addPath`, `Builder.addLabeledPath` and/or `Tron.Expose.Convert` helpers.

There is a special `Backed` program, that just stores the stringified values in the `Dict` and is able to send them to the JS, see [Backed](#Backed) section below.

# Program

@docs ProgramWithTron

# Program Wrappers
@docs sandbox, element, document, application

# Backed

_Backed_ is the special type of `Program` that only stores the mapping of path in the interface to the corresponding current value. This is useful when you want to use Tron GUI for some JS application and just send the updates using the ports.

For such cases, see `ReportToJsBacked` example. Another examples that use JS connection and ports, but don't use `Backed` helper, are: `ReportToJsJson`, `ReportToJsString`, `DatGui`.

@docs backed, BackedStorage, BackedWithTron
-}


import Browser exposing (UrlRequest(..))
import Url exposing (Url)
import Html exposing (Html)
--import Either exposing (Either(..))
import Random
import Dict exposing (Dict)
import Task

import Tron.Core as Core exposing (Model)
import Tron exposing (Tron)
--import Tron.Builder as Builder exposing (Builder)
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock exposing (Dock(..))
import Tron.Expose as Exp
import Tron.Expose.Convert as Exp
import Tron.Option exposing (..)
import Tron.Msg exposing (Msg_(..))
import Tron.Detach as Detach
import Tron.Property as Property exposing (LabelPath)
import Tron.Expose.Data as Exp


{-| Adds `Model msg` to the Elm `Program` and so controls all the required communication between usual App and GUI. -}
type alias ProgramWithTron flags model msg =
    Program flags ( model, Model msg ) (WithTronMsg msg)


type WithTronMsg msg
    = ToUser msg
    | ToTron Core.Msg
    --| Ack Exp.Ack
    | SendUpdate Exp.RawOutUpdate
    | ReceiveRaw Exp.RawInUpdate
    | SetClientId Detach.ClientId
    | UrlChange (Maybe msg) Url


init
    :  ( flags -> ( model, Cmd msg ), model -> Tron msg )
    -> Maybe Url
    -> RenderTarget
    -> PortCommunication msg
    -> flags
    -> ( ( model, Model msg ), Cmd (WithTronMsg msg) )
init ( userInit, userFor ) maybeUrl renderTarget ports flags =
    let
        ( initialModel, userEffect ) =
            userInit flags
        ( gui, guiEffect ) =
            userFor initialModel
                |> Core.init
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
                Just url ->
                    Task.succeed url
                        |> Task.perform (UrlChange Nothing)
                Nothing -> Cmd.none
            ]
        )


view
    :  (model -> Html msg)
    -> RenderTarget
    -> (model, Model msg)
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
    -> ( model, Model msg )
    -> Sub (WithTronMsg msg)
subscriptions userSubscriptions ports ( model, gui ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map ToUser
        , Core.subscriptions gui |> Sub.map ToTron
        , addSubscriptionsOptions ports |> Sub.map ReceiveRaw
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Tron msg )
    -> PortCommunication msg
    -> WithTronMsg msg
    -> ( model, Model msg )
    -> ( ( model, Model msg ), Cmd (WithTronMsg msg) )
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
                    |> Core.over (userFor newUserModel)
                )
            , userEffect |> Cmd.map ToUser
            )

        ToTron guiMsg ->
            case gui |> Core.toExposed |> Core.update guiMsg of
                ( nextGui, guiEffect ) ->
                    (
                        ( model
                        , nextGui |> Core.map Tuple.second
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
            , case ports of
                Detachable { ack } ->
                    Exp.encodeAck clientId
                        |> ack
                        |> Cmd.map ToUser
                _ -> Cmd.none
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


performInitEffects : PortCommunication msg -> Model msg -> Cmd msg
performInitEffects ports gui =
        case ports of
            SendJson { ack } ->
                gui |> Core.encode |> ack
            DatGui { ack } ->
                gui |> Core.encode |> ack
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
                ( rawUpdate.labelPath
                , rawUpdate.stringValue
                )
        _ -> Cmd.none


addInitOptions : RenderTarget -> Model msg -> Model msg
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


useRenderTarget : RenderTarget -> Model msg -> Html Core.Msg
useRenderTarget target gui =
    case target of
        Html dock theme -> gui |> Core.dock dock |> Core.view theme
        Nowhere -> Html.div [] []
        Aframe _ -> Html.div [] [] -- FIXME


setDetachState : ( Maybe Detach.ClientId, Detach.State ) -> Model msg -> Model msg
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


{-| Wrapper for `Program.sandbox` with `for` function and `Tron` options. -}
sandbox
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Tron msg
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


{-| Wrapper for `Program.element` with `for` function and `Tron` options. -}
element
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Tron msg
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


{-| Wrapper for `Program.document` with `for` function and `Tron` options. -}
document
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Tron msg
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


{-| Wrapper for `Program.application` with `for` function and `Tron` options. -}
application
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Tron msg
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


{-| Path-to-value storage, to transmit them to the JS side. -}
type alias BackedStorage = Dict LabelPath String


type alias BackedMsg = ( LabelPath, String ) -- message is just key & value to put in the dict


{-| Program, backed with the path-to-value storage. -}
type alias BackedWithTron = ProgramWithTron () BackedStorage BackedMsg


{-| Special helper, that just asks you for the `Unit`-based interface and some port where to send value updates as `(String, String) pairs, which are labeled path and the stringified value.

See `Tron.Builder.Unit` for the functions that will help you define the interface that only operates `()` values and so doesn't need any messages.

    import WithTron exposing (BackedWithTron)
    import Tron.Builder.Unit exposing (..)

    gui : Float -> Tron
    gui amount =
        Builder.root
            [
                ( "amount"
                , Builder.float
                    { min = 0, max = 1, step = 0.01 }
                    amount
                )
            ]

    port sendUpdate : ( String, String ) -> Cmd msg

    main : BackedWithTron
    main =
        WithTron.backed
            (Option.toHtml Dock.middleRight Theme.dark)
            sendUpdate
            gui


See `example/ReportToJsBacked` for more details.

 -}
backed
    :  RenderTarget
    -> (( List String, String ) -> Cmd msg)
    -> Tron ()
    -> BackedWithTron
backed renderTarget transmit tree =
    let

        tree_ : Tron BackedMsg
        tree_ = tree |> Exp.toStrExposed |> Property.map Tuple.first

        for_ : BackedStorage -> Tron BackedMsg
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
