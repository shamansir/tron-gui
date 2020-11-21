port module Detachable.Main exposing (main)


import Url exposing (Url)

import Browser exposing (element)
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Gui as Tron exposing (Gui, init, view, update, subscriptions)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Detach as Detach exposing (fromUrl)
import Gui.Expose as Exp exposing (RawProperty, RawUpdate)

import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui


type Msg
    = NoOp
    | ToExample Example.Msg
    | ToTron Tron.Msg


type alias Model =
    ( Example.Model, Tron.Gui Msg )


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url _ =
    let
        example = Example.init
        ( gui, guiEffect ) =
            ExampleGui.for example
                |> Tron.init
        ( nextGui, detachableEffect ) =
            gui
                |> Tron.detachable
                    url
                    ackToWs
                    sendUpdateToWs
                    receieveUpdateFromWs
    in
        (
            ( example
            , nextGui
                |> Tron.map ToExample
            )
        , Cmd.batch
            [ guiEffect
            , detachableEffect
            ]
            |> Cmd.map ToTron
        )


view : Model -> Html Msg
view ( example, gui ) =
    Html.div
        [ ]
        [ gui
            |> Tron.view Theme.light
            |> Html.map ToTron
        , Example.view example
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( example, gui ) =
    case msg of

        NoOp ->
            ( ( example, gui )
            , Cmd.none
            )

        ToExample dmsg ->
            (
                ( example |> Example.update dmsg
                , gui
                )
            , Cmd.none
            )

        ToTron guiMsg ->
            case gui |> Tron.update guiMsg of
                ( nextGui, cmds ) ->
                    (
                        ( example
                        , nextGui
                        )
                    , cmds
                    )


subscriptions : Model -> Sub Msg
subscriptions ( _, gui ) =
    Tron.subscriptions gui |> Sub.map ToTron


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


port receieveUpdateFromWs : (Exp.RawUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawUpdate -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
