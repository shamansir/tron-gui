port module Everything.Main exposing (main)


import Browser
import Browser.Events as Browser
import Browser.Dom as Browser
import Browser.Navigation as Navigation
import Json.Decode as Decode
import Json.Decode as D
import Json.Encode as Encode
import Html exposing (Html)
import Html as Html exposing (map, div)
import Html.Attributes as Attr exposing (class)
import Html.Events as Html exposing (onClick)
import Dict exposing (size)
import Task as Task
import Random
import Url exposing (Url)

import Gui exposing (Gui)
import Gui as Gui exposing (view, detachable, subscriptions)
import Gui.Expose as Exp exposing (Update)
import Gui as Tron exposing (Gui)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Mouse exposing (Position)
import Gui.Build as Tron exposing (Builder)
import Gui.Build as Builder exposing (map)
import Gui.Detach as Detach exposing (fromUrl)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Theme as Theme
import Gui.Style.Dock exposing (Dock)
import Gui.Style.Dock as Dock

import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


{-
-- Change to a more boring example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui
-}

import RandomGui as Gui exposing (generator)


type Msg
    = NoOp
    | ChangeMode Mode
    | ChangeDock Dock
    | FromDatGui Exp.RawUpdate
    | ToTron Tron.Msg
    | ToExample Example.Msg
    | Randomize (Tron.Builder ())
    | SwitchTheme
    | TriggerRandom
    | TriggerDefault


type Mode
    = DatGui
    | TronGui


type alias Model =
    { mode : Mode
    , theme : Theme
    , gui : Tron.Gui Msg
    , example : Example.Model
    , url : Url
    }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url _ =
    let
        initialModel = Example.init
        ( gui, startGui ) =
            initialModel
                |> exampleGui url
    in
        (
            { mode = TronGui
            , example = initialModel
            , theme = Theme.light
            , gui = gui
                |> Gui.reshape ( 7, 7 )
                |> Gui.redock Dock.bottomLeft
            , url = url
            }
        , startGui
        )


view : Model -> Html Msg
view { mode, gui, example, theme } =
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
                gui
                    |> Gui.view theme
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
            , model.gui
                |> Tron.encode
                |> startDatGui
            )

        ( ChangeMode TronGui, _ ) ->
            let
                ( gui, startGui ) =
                    model.example |> exampleGui model.url
            in
                (
                    { model
                    | gui = gui
                    , mode = TronGui
                    }
                , Cmd.batch
                    [ destroyDatGui ()
                    , startGui
                    ]
                )

        ( ToExample dmsg, _ ) ->
            (
                -- If your GUI structure never changes (unlike with `Goose` example),
                -- you need neither `updatedBy` function nor this condition check,
                -- at all! Just leave the `else` part in your code.
                if ExampleGui.updatedBy dmsg then

                    let nextExample = model.example |> Example.update dmsg
                    in
                        { model
                        | example = nextExample
                        , gui =
                            model.gui
                                |> Tron.over
                                    (ExampleGui.for nextExample
                                        |> Builder.map ToExample)
                        }
                else
                    { model
                    | example =
                        Example.update dmsg model.example
                    }
            , Cmd.none
            )

        ( ToTron guiMsg, TronGui ) ->
            case model.gui |> Gui.update guiMsg of
                ( nextGui, cmds ) ->
                    (
                        { model
                        | gui = nextGui
                        }
                    , cmds
                    )

        ( ToTron _, DatGui ) -> ( model, Cmd.none )

        ( FromDatGui guiUpdate, DatGui ) ->
            ( model
            , model.gui
                |> Tron.applyRaw guiUpdate
            )

        ( FromDatGui _, TronGui ) -> ( model, Cmd.none )

        ( TriggerRandom, _ ) ->
            ( model
            , Cmd.batch
                [ destroyDatGui ()
                , Gui.generator
                    |> Random.generate Randomize
                ]
            )

        ( Randomize newTree, _ ) ->
            let
                ( newGui, startGui ) =
                    newTree |> Gui.init
            in
                (
                    { model
                    | gui = newGui
                        |> Gui.map (always NoOp)
                        |> Gui.redock Dock.bottomCenter
                    }
                , case model.mode of
                    DatGui ->
                        newGui
                            |> Tron.encode
                            |> startDatGui
                    TronGui ->
                        startGui
                            |> Cmd.map ToTron
                )

        ( TriggerDefault, _ ) ->
            let
                ( gui, startGui ) =
                    Example.init |> exampleGui model.url
            in
                (
                    { model
                    | gui = gui
                    }
                , startGui
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
                | gui = model.gui |> Gui.redock newDock
                }
            , Cmd.none
            )

        ( NoOp, _ ) -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { mode, gui } =
    case mode of
        DatGui ->
            updateFromDatGui FromDatGui
        TronGui ->
            Gui.subscriptions gui |> Sub.map ToTron


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


exampleGui : Url -> Example.Model -> ( Gui Msg, Cmd Msg )
exampleGui url model =
    let
        ( gui, startGui ) =
            ExampleGui.for model
                |> Gui.init
        ( nextGui, launchDetachable )
            = gui
                |> Gui.detachable
                    url
                    ackToWs
                    sendUpdateToWs
                    receieveUpdateFromWs
    in
        ( nextGui
            |> Gui.map ToExample
        , Cmd.batch
            [ startGui
            , launchDetachable
            ]
            |> Cmd.map ToTron
        )


port startDatGui : Exp.RawProperty -> Cmd msg

port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port destroyDatGui : () -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg

port receieveUpdateFromWs : (Exp.RawUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawUpdate -> Cmd msg
