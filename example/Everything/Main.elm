port module Everything.Main exposing (main)


import Browser
import Browser.Events as Browser
import Browser.Dom as Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
-- import Html as Html exposing (map, div)
import Html.Attributes as Attr
import Html.Events as Html
-- import Dict exposing (size)
-- import Task as Task
import Random
import Url exposing (Url)

import WithTron.Logic as WithTron
import Tron exposing (Tron)
import Tron.Deferred as Def
import Tron.Core as Core
import Tron.Core as T
import Tron.Option as Option
import Tron.Expose as Exp
import Tron.Expose.Data as Exp
import Tron.Expose.Convert as Exp
import Tron.Mouse exposing (Position)
import Tron.Detach as Detach exposing (fromUrl)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Theme as Theme
import Tron.Style.Dock exposing (Dock)
import Tron.Style.Dock as Dock
import Tron.Property as Property

import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


{-
-- Change to `Default` example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui
-}


{-

IMPORTANT:

The code in this example is a collection of all the possible Tron features + random GUI generation.
Switching these deatures during the usual application cycle is not supported by public Tron API,
trying to be as minimalistic as possible. Hence the code here is very complex and unfriendly.

Please, don't treat it as the sample code of Tron usage.

For the actual examples of using Tron in your applications, see other examples nearby.
-}


import RandomGui as Gui exposing (generator)


type Msg
    = NoOp
    | SetClientId Detach.ClientId
    | ChangeMode Mode
    | ChangeDock Dock
    | SendUpdate Exp.RawOutUpdate
    | ReceiveRaw Exp.RawInUpdate
    | ToTron Core.Msg
    | ToExample Example.Msg
    | Randomize (Tron ())
    | SwitchTheme
    | TriggerRandom
    | TriggerDefault


type Mode
    = DatGui
    | TronGui


type alias Model =
    { mode : Mode
    , theme : Theme
    , state : Core.State
    , lastGui : Tron ()
    , example : Example.Model
    , url : Url
    , isRandom : Bool
    }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url _ =
    let
        ( initialModel, initEffects ) = Example.init
        ( state, startGui ) = Core.init
        initialGui =
            ExampleGui.for initialModel
                    |> Tron.toUnit
    in
        (
            { mode = TronGui
            , example = initialModel
            , theme = Theme.light
            , state =
                state
                    |> Core.reshape ( 7, 7 )
                    |> Core.dock Dock.bottomLeft
            , lastGui =
                ExampleGui.for initialModel
                    |> Tron.toUnit
            , url = url
            , isRandom = False
            }
        , Cmd.batch
            [ initEffects |> Cmd.map ToExample
            , startGui |> Cmd.map ToTron
            , WithTron.performInitEffects portCommunication initialGui
            , applyUrl portCommunication url
            ]
        )


view : Model -> Html Msg
view { mode, state, example, lastGui, theme } =
    Html.div
        [ Attr.class <| "example --" ++ Theme.toString theme ]
        [ Html.button
            [ Html.onClick <| ChangeMode TronGui ]
            [ Html.text "Tron" ]
        , Html.button
            [ Html.onClick <| ChangeMode DatGui ]
            [ Html.text "Dat.gui" ]
        , Html.button
            [ Html.onClick TriggerRandom ]
            [ Html.text "Random" ]
        , Html.button
            [ Html.onClick TriggerDefault ]
            [ Html.text "Default" ]
        , Html.button
            [ Html.onClick SwitchTheme ]
            [ Html.text "Theme" ]
        , Html.span
            []
            <| List.map
                (\(label, dock) ->
                    Html.button
                        [ Html.onClick <| ChangeDock dock ]
                        [ Html.text label ]
                )
            <|
            [ ( "⬁", Dock.topLeft )
            , ( "⇧", Dock.topCenter )
            , ( "⬀", Dock.topRight )
            , ( "⇦", Dock.middleLeft )
            , ( "◯", Dock.center )
            , ( "⇨", Dock.middleRight )
            , ( "⬃", Dock.bottomLeft )
            , ( "⇩", Dock.bottomCenter )
            , ( "⬂", Dock.bottomRight )
            ]
        , case mode of
            DatGui -> Html.div [] []
            TronGui ->
                lastGui
                    |> Core.view theme state
                    |> Html.map ToTron
        , Example.view example
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.mode ) of

        ( ChangeMode DatGui, _ ) ->
            (
                { model
                | mode = DatGui
                }
            ,
                model.lastGui
                    |> Exp.encode
                    |> startDatGui
            )

        ( ChangeMode TronGui, _ ) ->
            let
                ( state, startGui ) =
                    Core.init
            in
                (
                    { model
                    | state = state
                    , mode = TronGui
                    }
                , Cmd.batch
                    [ destroyDatGui ()
                    , startGui |> Cmd.map ToTron
                    ]
                )

        ( SetClientId clientId, _ ) ->
            (
                let
                    curState = model.state
                    newState =
                        { curState
                        | detach =
                            case curState.detach of
                                ( _, detachState ) -> ( Just clientId, detachState )
                        }
                in
                    { model
                    | state = newState
                    }
            , case portCommunication of
                Option.Detachable { ack } ->
                    Exp.encodeAck clientId
                        |> ack
                _ -> Cmd.none
            )

        ( SendUpdate rawUpdate, _ ) ->
            ( model
            , rawUpdate
                |> WithTron.tryTransmitting portCommunication
            )

        ( ToExample dmsg, _ ) ->
            let
                ( nextExample, updateEffects ) =
                    model.example |> Example.update dmsg
            in
                (
                    { model
                    | example = nextExample
                    , lastGui =
                        if model.isRandom
                            then model.lastGui
                            else
                                ExampleGui.for nextExample
                                    |> Property.transferTransientState model.lastGui
                                    |> Property.loadValues model.lastGui
                                    |> Tron.toUnit
                    }
                , updateEffects |> Cmd.map ToExample
                )

        ( ToTron guiMsg, TronGui ) ->
            let
                (nextState, nextGui, guiEffect) =
                    ( if model.isRandom
                        then
                            model.lastGui
                                |> Def.lift
                                |> Def.map (always NoOp)
                        else
                            ExampleGui.for model.example
                                |> Property.transferTransientState model.lastGui
                                |> Property.loadValues model.lastGui
                                |> Def.map ToExample
                    ) |> Core.update guiMsg model.state
            in
                (
                    { model
                    | state = nextState
                    , lastGui = nextGui
                    }
                , guiEffect |> Cmd.map Tuple.second
                )

        ( ToTron _, DatGui ) -> ( model, Cmd.none )

        -- ( FromDatGui guiUpdate, DatGui ) ->
        ( ReceiveRaw guiUpdate, DatGui ) ->
            ( model
            ,
                (if model.isRandom
                    then
                        model.lastGui
                            |> Def.lift
                            |> Def.map (always NoOp)
                    else
                        ExampleGui.for model.example
                            |> Def.map ToExample
                )
                |> Core.applyRaw guiUpdate
            )

        ( ReceiveRaw _, TronGui ) -> ( model, Cmd.none )

        ( TriggerRandom, _ ) ->
            ( model
            , Cmd.batch
                [ destroyDatGui ()
                , Gui.generator
                    |> Random.generate Randomize
                ]
            )

        ( Randomize newTree, _ ) ->
            (
                { model
                | isRandom = True
                , lastGui = newTree
                }
            , case model.mode of
                DatGui ->
                    newTree
                        |> Exp.encode
                        |> startDatGui
                TronGui ->
                    WithTron.performInitEffects portCommunication newTree
            )

        ( TriggerDefault, _ ) ->
            let
                ( example, exampleEffects ) =
                    Example.init
                -- ( defaultState, startGui ) =
                --     Core.init
                --     -- example |> exampleGui model.url
            in
                (
                    { model
                    | example = example
                    , isRandom = False
                    }
                , Cmd.batch
                    [ exampleEffects |> Cmd.map ToExample
                    --, startGui |> Cmd.map ToTron
                    ]
                )

        ( SwitchTheme, _ ) ->
            (
                { model
                | theme = Theme.toggle model.theme
                }
            , Cmd.none
            )

        ( ChangeDock newDock, _ ) ->
            (
                { model
                | state = model.state |> Core.dock newDock
                }
            , Cmd.none
            )

        ( NoOp, _ ) -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { mode, state } =
    case mode of
        DatGui ->
            updateFromDatGui ReceiveRaw
        TronGui ->
            Sub.batch
                [ Core.subscriptions state |> Sub.map ToTron
                , WithTron.addSubscriptionsOptions portCommunication |> Sub.map ReceiveRaw
                ]


main : Program () Model Msg
main =
    Browser.application
        { init = always init
        , view = \model ->
            { title = "Tron GUI"
            , body = [ view model ]
            }
        , subscriptions = subscriptions
        , update = update
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


portCommunication : Option.PortCommunication msg
portCommunication =
    Option.Detachable
        { ack = ackToWs
        , transmit = sendUpdateToWs
        , receive = receieveUpdateFromWs identity
        }


-- See WithTron.applyUrl
applyUrl
    :  Option.PortCommunication msg
    -> Url.Url
    -> Cmd Msg
applyUrl ports url =
    let
        ( maybeClientId, state ) = Detach.fromUrl url
    in
        case ( ports, maybeClientId ) of
            ( Option.Detachable { ack }, Just clientId ) ->
                Exp.encodeAck clientId
                    |> ack
                    |> Cmd.map (always NoOp)
            ( Option.Detachable { ack }, Nothing ) ->
                WithTron.nextClientId
                    |> Cmd.map SetClientId

            _ -> Cmd.none


{-
exampleGui : Url -> Example.Model -> ( Core.Model Msg, Cmd Msg )
exampleGui url model =
    let
        ( gui, startGui ) =
            ExampleGui.for model
                |> Core.init
        ( nextGui, launchDetachable )
            = ( gui, Cmd.none )
                -- |> Gui.detachable FIXME
                --     url
                --     ackToWs
                --     sendUpdateToWs
                --     receieveUpdateFromWs
    in
        ( nextGui
            |> Core.map ToExample
        , Cmd.batch
            [ startGui
            , launchDetachable
            ]
            |> Cmd.map ToTron
        )
-}


port startDatGui : Exp.RawProperty -> Cmd msg

port updateFromDatGui : (Exp.RawInUpdate -> msg) -> Sub msg

port destroyDatGui : () -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg

port receieveUpdateFromWs : (Exp.RawInUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawOutUpdate -> Cmd msg
