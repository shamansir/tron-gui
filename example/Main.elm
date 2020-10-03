port module Main exposing (main)


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
import Gui.Gui as Gui exposing (view, detachable)
import Gui.Expose as Exp exposing (Update)
import Gui.Gui as Tron exposing (Gui, focus, over)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Mouse exposing (Position)
import Gui.Render.Style as Style exposing (..)

import Simple.Main as Simple
import Simple.Model as Simple
import Simple.Msg as Simple
import Simple.Gui as SimpleGui

import RandomGui as Gui exposing (generator)



uuid = "b2465319"


type Msg
    = NoOp
    | ChangeMode Mode
    | Resize Int Int
    | DatGuiUpdate Exp.Update
    | TronUpdate Tron.Msg
    | ToSimple Simple.Msg
    | Randomize (Tron.Gui Msg)
    | SwitchTheme
    | TriggerRandom
    | TriggerDefault


type alias Example
    = Simple.Model


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
    (
        { mode = TronGui
        , example = Simple.init
        , theme = Style.Light
        , gui = Gui.init Gui.TopToBottom
                    |> Gui.over (SimpleGui.for Simple.init)
                    |> Gui.detachable sendUiToWs sendUpdateToWs url
                    |> Gui.map ToSimple
        }
    , Cmd.batch -- FIXME: Gui.init
        [ Tron.focus NoOp
        , Tron.fromWindow Resize -- FIXME: subscribe to resizes along with the mouse
        ]
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
        , Simple.view example
            |> Html.map (always NoOp)
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
                |> .tree -- FIXME
                |> Exp.encode
                |> startDatGui
            )

        ( ChangeMode TronGui, _ ) ->
            (
                { model
                | mode = TronGui
                }
            , Cmd.batch
                [ destroyDatGui ()
                , Tron.focus NoOp
                , Tron.fromWindow Resize
                ]
            )

        ( ToSimple smsg, _ ) ->
            (
                { model
                | example =
                    Simple.update smsg model.example
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
                |> .tree -- FIXME
                |> Exp.update guiUpdate
            )

        ( Resize width height, _ ) ->
            (
                { model
                | gui = model.gui |> Gui.reflow ( width, height )
                }
            , Cmd.none
            )

        ( TriggerRandom, _ ) ->
            ( model
            , Cmd.batch
                [ destroyDatGui ()
                , Random.generate Randomize
                    <| Random.map (Gui.map <| always NoOp)
                    <| Random.map
                        (\prop ->
                            Gui.init Gui.TopToBottom |> Gui.over prop
                        )
                    <| Gui.generator
                ]
            )

        ( Randomize newGui, _ ) ->
            (
                { model
                | gui = newGui
                }
            , case model.mode of
                DatGui ->
                    newGui
                        |> .tree -- FIXME
                        |> Exp.encode
                        |> startDatGui
                TronGui ->
                    Cmd.batch
                        [ Tron.focus NoOp
                        , Tron.fromWindow Resize -- FIXME: subscribe to resizes along with the mouse
                        ]
            )

        ( TriggerDefault, _ ) ->
            (
                { model
                | gui =
                    model.gui
                        |> Gui.overMap ToSimple (SimpleGui.for Simple.init)
                }
            , Cmd.batch -- FIXME: Gui.init
                [ Tron.focus NoOp
                , Tron.fromWindow Resize -- FIXME: subscribe to resizes along with the mouse
                ]
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
subscriptions { mode } =
    case mode of
        DatGui ->
            updateFromDatGui (DatGuiUpdate << Exp.fromPort)
        TronGui ->
            Sub.batch
                [ Gui.trackMouse |> Sub.map TronUpdate
                , receieveUpdateFromWs (TronUpdate << Tron.ReceiveRaw)
                , Browser.onResize Resize
                ]


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


port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port startDatGui : Exp.RawProperty -> Cmd msg

port destroyDatGui : () -> Cmd msg

port receieveUiFromWs : (Exp.RawProperty -> msg) -> Sub msg

port receieveUpdateFromWs : (Exp.RawUpdate -> msg) -> Sub msg

-- port updateDatGui : Encode.Value -> Cmd msg

port sendUiToWs : Exp.RawProperty -> Cmd msg

port sendUpdateToWs : Exp.RawUpdate -> Cmd msg
