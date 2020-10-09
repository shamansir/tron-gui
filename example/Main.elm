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
import Gui.Gui as Gui exposing (view, detachable, subscribe)
import Gui.Expose as Exp exposing (Update)
import Gui.Gui as Tron exposing (Gui, focus, over)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Mouse exposing (Position)
import Gui.Render.Style as Style exposing (..)

import Default.Main as Default
import Default.Model as Default
import Default.Msg as Default
import Default.Gui as DefaultGui

import RandomGui as Gui exposing (generator)


type Msg
    = NoOp
    | ChangeMode Mode
    | DatGuiUpdate Exp.Update
    | TronUpdate Tron.Msg
    | ToDefault Default.Msg
    | Randomize (Tron.Gui Msg)
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
    (
        { mode = TronGui
        , example = Default.init
        , theme = Style.Light
        , gui = Gui.init Gui.TopToBottom
                    |> Gui.over (DefaultGui.for Default.init)
                    |> Gui.detachable sendUpdateToWs receieveUpdateFromWs url
                    |> Gui.map ToDefault
        }
    , Tron.run |> Cmd.map TronUpdate
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
                , Tron.run |> Cmd.map TronUpdate
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
                |> .tree -- FIXME
                |> Exp.update guiUpdate
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
                    Tron.run |> Cmd.map TronUpdate
            )

        ( TriggerDefault, _ ) ->
            (
                { model
                | gui =
                    model.gui
                        |> Gui.overMap ToDefault (DefaultGui.for Default.init)
                }
            , Tron.run |> Cmd.map TronUpdate
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
            updateFromDatGui (DatGuiUpdate << Exp.fromPort)
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


port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port startDatGui : Exp.RawProperty -> Cmd msg

port destroyDatGui : () -> Cmd msg

port receieveUpdateFromWs : (Exp.RawUpdate -> msg) -> Sub msg

-- port updateDatGui : Encode.Value -> Cmd msg

port sendUpdateToWs : Exp.RawUpdate -> Cmd msg
