module WithTron exposing
    ( ProgramWithTron
    , sandbox, element, document, application
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

*NB*: Please don't forget to add a copy of `src/Tron.css` to your application, or refer to one hosted at GitHub.

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

See also: `WithTron.Backed`.
-}


import Browser exposing (UrlRequest(..))
import Url exposing (Url)
import Html exposing (Html)
import Random
import Dict exposing (Dict)
import Dict.Extra as Dict
import Task

import Tron.Core as Core exposing (State)
import Tron exposing (Tron)
import TronRef as Ref
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
    Program flags ( model, State, Tron () ) ( WithTronMsg msg )


type WithTronMsg msg
    = ToUser msg
    | ToTron Core.Msg
    --| Ack Exp.Ack
    | SendUpdate Exp.RawOutUpdate
    | ReceiveRaw Exp.RawInUpdate
    | SetClientId Detach.ClientId
    | UrlChange (Maybe msg) Url


init
    :  ( flags -> ( model, Cmd msg ), model -> Tron () )
    -> Maybe Url
    -> RenderTarget
    -> PortCommunication msg
    -> flags
    -> ( ( model, State, Tron () ), Cmd (WithTronMsg msg) )
init ( userInit, userFor ) maybeUrl renderTarget ports flags =
    let
        ( initialModel, userEffect ) =
            userInit flags
        ( state, guiEffect ) =
            Core.init
        firstTree =
            userFor initialModel
    in
        (
            ( initialModel
            , state |> addInitOptions renderTarget
            , firstTree |> Core.invalidate
            )
        , Cmd.batch
            [ userEffect |> Cmd.map ToUser
            , guiEffect |> Cmd.map ToTron
            , performInitEffects ports (firstTree |> Core.invalidate) |> Cmd.map ToUser
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
    -> ( model, State, Tron () )
    -> Html (WithTronMsg msg)
view userView renderTarget ( model, state, tree ) =
    Html.div
        [ ]
        [ tree
            |> useRenderTarget renderTarget state
            |> Html.map ToTron
        , userView model
            |> Html.map ToUser
        ]


subscriptions
    :  ( model -> Sub msg )
    -> PortCommunication msg
    -> ( model, State, Tron () )
    -> Sub (WithTronMsg msg)
subscriptions userSubscriptions ports ( model, state, _ ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map ToUser
        , Core.subscriptions state |> Sub.map ToTron
        , addSubscriptionsOptions ports |> Sub.map ReceiveRaw
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Ref.Tron msg )
    -> PortCommunication msg
    -> WithTronMsg msg
    -> ( model, State, Tron () )
    -> ( ( model, State, Tron () ), Cmd (WithTronMsg msg) )
update ( userUpdate, userFor ) ports withTronMsg ( model, state, prevTree ) =
    case withTronMsg of

        ToUser userMsg ->
            let
                ( newUserModel, userEffect ) =
                    userUpdate userMsg model
            in

            (
                ( newUserModel
                , state
                , userFor newUserModel
                    |> Property.transferTransientState prevTree
                    |> Property.loadValues prevTree
                    |> Core.invalidate
                )
            , userEffect |> Cmd.map ToUser
            )

        ToTron guiMsg ->
            case prevTree
                    |> Exp.lift
                    |> Core.update guiMsg state of
                ( nextState, nextTree, guiEffect ) ->
                    (
                        ( model
                        , nextState
                        , nextTree
                        )
                    , Cmd.batch
                        [ {- guiEffect
                            |> Cmd.map ToUser -}
                        ]

                    )

        ReceiveRaw rawUpdate ->
            let
                nextRoot =
                    userFor model
                        |> Property.transferTransientState prevTree
                        |> Exp.apply (Exp.fromPort rawUpdate)
            in
                (
                    ( model
                    , state
                    , nextRoot |> Core.invalidate
                    )
                , nextRoot
                    |> Exp.freshRun
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
            , case ports of
                Detachable { ack } ->
                    Exp.encodeAck clientId
                        |> ack
                        |> Cmd.map ToUser
                _ -> Cmd.none
            )

        SendUpdate rawUpdate ->
            ( ( model, state, prevTree )
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
                ( rawUpdate.labelPath
                , rawUpdate.stringValue
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


{-| Wrapper for `Program.sandbox` with `for` function and `Tron` options.

For example:

    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Option as Option
    import WithTron exposing (ProgramWithTron)

    main : ProgramWithTron () Example.Model Example.Msg
    main =
        WithTron.sandbox
            (Option.toHtml Dock.center Theme.dark)
            Option.noCommunication
            { for = ExampleGui.for
            , init = Example.init
            , view = Example.view
            , update = Example.update
            }


-}
sandbox
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Ref.Tron msg
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


{-| Wrapper for `Program.element` with `for` function and `Tron` options.

Example from `Basic/Main.elm`

    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Option as Option
    import WithTron exposing (ProgramWithTron)

    import Example.Goose.Main as Example
    import Example.Goose.Model as Example
    import Example.Goose.Msg as Example
    import Example.Goose.Gui as ExampleGui

    main : ProgramWithTron () Example.Model Example.Msg
    main =
        WithTron.element
            (Option.toHtml Dock.center Theme.dark)
            Option.noCommunication
            { for = ExampleGui.for
            , init = always Example.init
            , view = Example.view
            , update = Example.update
            , subscriptions = always Sub.none
            }

 -}
element
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Ref.Tron msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> ProgramWithTron flags model msg
element renderTarget ports def =
    Browser.element
        { init =
            init ( def.init, def.for >> Core.invalidate ) Nothing renderTarget ports
        , update =
            update ( def.update, def.for ) ports
        , view =
            view def.view renderTarget
        , subscriptions =
            subscriptions def.subscriptions ports
        }


{-| Wrapper for `Program.document` with `for` function and `Tron` options.

For example:

    import WithTron exposing (ProgramWithTron)
    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Expose.Data as Exp
    import Tron.Option as Option

    main : ProgramWithTron () Example.Model Example.Msg
    main =
        WithTron.document
            (Option.toHtml Dock.center Theme.light)
            (Option.detachable
                { ack = ackToWs
                , transmit = sendUpdateToWs
                , receive = receieveUpdateFromWs identity
                }
            )
            { for = ExampleGui.for
            , init = always Example.init
            , view =
                \model ->
                    { title = "Detachable Tron"
                    , body = [ Example.view model ]
                    }
            , update = Example.update
            , subscriptions = always Sub.none
            }

 -}
document
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Ref.Tron msg
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
                init ( def.init, def.for >> Core.invalidate) Nothing renderTarget ports flags
        , update =
            update ( def.update, def.for ) ports
        , view =
            \(userModel, state, tree) ->
                { title = (def.view userModel).title
                , body =
                    [ view
                        (\umodel ->
                            -- FIXME: we calculate view two times, it seems
                            Html.div [] <| (def.view umodel).body
                        )
                        renderTarget
                        (userModel, state, tree)
                    ]
                }
        , subscriptions =
            subscriptions def.subscriptions ports
        }


{-| Wrapper for `Program.application` with `for` function and `Tron` options.

Example from `Detachable/Main.elm`:

    import WithTron exposing (ProgramWithTron)
    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Expose.Data as Exp
    import Tron.Option as Option

    import Example.Goose.Main as Example
    import Example.Goose.Model as Example
    import Example.Goose.Msg as Example
    import Example.Goose.Gui as ExampleGui

    main : ProgramWithTron () Example.Model Example.Msg
    main =
        WithTron.application
            (Option.toHtml Dock.center Theme.light)
            (Option.detachable
                { ack = ackToWs
                , transmit = sendUpdateToWs
                , receive = receieveUpdateFromWs identity
                }
            )
            { for = ExampleGui.for
            , init = always Example.init
            , view =
                \model ->
                    { title = "Detachable Tron"
                    , body = [ Example.view model ]
                    }
            , update = Example.update
            , subscriptions = always Sub.none
            , onUrlChange = always Example.NoOp
            , onUrlRequest = always Example.NoOp
            }

    port receieveUpdateFromWs : (Exp.RawInUpdate -> msg) -> Sub msg

    port sendUpdateToWs : Exp.RawOutUpdate -> Cmd msg

    port ackToWs : Exp.Ack -> Cmd msg

 -}
application
    :  RenderTarget
    -> PortCommunication msg
    ->
        { for : model -> Ref.Tron msg
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
                init ( def.init, def.for >> Core.invalidate ) (Just url) renderTarget ports flags
        , update =
            update ( def.update, def.for ) ports
        , view =
            \(userModel, state, tree) ->
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
                            (userModel, state, tree)
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


