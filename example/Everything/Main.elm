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

import Gui.Gui exposing (Gui)
import Gui.Gui as Gui exposing (view, detachable, subscribe)
import Gui.Expose as Exp exposing (Update)
import Gui.Gui as Tron exposing (Gui)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Mouse exposing (Position)
import Gui.Render.Style as Style exposing (..)
import Gui.Build as Tron exposing (Builder)
import Gui.Detach as Detach exposing (fromUrl)

import Default.Main as Default
import Default.Model as Default
import Default.Msg as Default
import Default.Gui as DefaultGui

import RandomGui as Gui exposing (generator)


type Msg
    = NoOp
    | ChangeMode Mode
    | DatGuiUpdate Exp.RawUpdate
    | TronUpdate Tron.Msg
    | ToDefault Default.Msg
    | Randomize (Tron.Builder ())
    | SwitchTheme
    | TriggerRandom
    | TriggerDefault


type alias Example
    = Default.Model


type Mode
    = DatGui
    | TronGui


type alias Model =
    { mode : Mode
    , theme : Style.Theme
    , gui : Tron.Gui Msg
    , example : Example
    }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url _ =
    let
        initialModel = Default.init
        ( gui, startGui ) =
            initialModel
                |> defaultGui (Detach.fromUrl url)
    in
        (
            { mode = TronGui
            , example = initialModel
            , theme = Style.Light
            , gui = gui
            }
        , startGui
        )


view : Model -> Html Msg
view { mode, gui, example, theme } =
    Html.div
        [ Attr.class <| "example " ++ case theme of
            Style.Dark -> "--dark"
            Style.Light -> "--light" ]
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
        , case mode of
            DatGui -> Html.div [] []
            TronGui ->
                gui
                    |> Gui.view theme
                    |> Html.map TronUpdate
        , Default.view example
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
                    model.example |> defaultGui Detach.attachedAtRoot
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

        ( ToDefault dmsg, _ ) ->
            (
                { model
                | example =
                    Default.update dmsg model.example
                }
            , Cmd.none
            )

        ( TronUpdate guiMsg, TronGui ) ->
            case model.gui |> Gui.update guiMsg of
                ( nextGui, cmds ) ->
                    (
                        { model
                        | gui = nextGui
                        }
                    , cmds
                    )

        ( DatGuiUpdate guiUpdate, DatGui ) ->
            ( model
            , model.gui
                |> Tron.applyRaw guiUpdate
            )

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
                    newTree
                        |> Gui.init Gui.TopToBottom
            in
                (
                    { model
                    | gui = newGui |> Gui.map (always NoOp)
                    }
                , case model.mode of
                    DatGui ->
                        newGui
                            |> Tron.encode
                            |> startDatGui
                    TronGui ->
                        startGui
                            |> Cmd.map TronUpdate
                )

        ( TriggerDefault, _ ) ->
            let
                ( gui, startGui ) =
                    Default.init |> defaultGui Detach.attachedAtRoot
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
                | theme = Style.switch model.theme
                }
            , Cmd.none
            )

        _ -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { mode, gui } =
    case mode of
        DatGui ->
            updateFromDatGui DatGuiUpdate
        TronGui ->
            Gui.subscribe gui |> Sub.map TronUpdate


main : Program () Model Msg
main =
    Browser.application
        { init = always init
        , view = \model ->
            { title = "Tron 2.0"
            , body = [ view model ]
            }
        , subscriptions = subscriptions
        , update = update
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


defaultGui : Detach.State -> Default.Model -> ( Gui Msg, Cmd Msg )
defaultGui detachState model =
    let
        ( gui, startGui ) =
            DefaultGui.for model
                |> Gui.init Gui.TopToBottom
    in
        ( gui
            |> Gui.detachable
                    detachState
                    sendUpdateToWs
                    receieveUpdateFromWs
            |> Gui.map ToDefault
        , startGui |> Cmd.map TronUpdate
        )


port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port startDatGui : Exp.RawProperty -> Cmd msg

port destroyDatGui : () -> Cmd msg

port receieveUpdateFromWs : (Exp.RawUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawUpdate -> Cmd msg
