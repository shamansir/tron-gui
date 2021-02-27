port module ReportToJsJson.Main exposing (main)


import Browser
import Html exposing (Html)

import Gui as Tron
import Gui.Style.Theme as Theme
import Gui.Expose as Exp

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


type Msg
    = ToTron Tron.Message
    | ToSend Exp.RawUpdate


type alias Model =
    Tron.Gui Exp.RawUpdate


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Example.init
        gui =
            ExampleGui.for example
                |> Exp.toExposed
                |> Tron.initRaw
    in
        ( gui
        , gui
            |> Tron.encode
            |> initGui
        )


view : Model -> Html Msg
view gui =
    Html.div
        [ ]
        [ Tron.view Theme.dark gui
            |> Html.map ToTron
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg gui =
    case msg of
        ToTron tronMsg ->
            let
                ( nextGui, updateToSend ) =
                    gui |> Tron.update tronMsg
            in
                ( nextGui
                , updateToSend
                    |> Cmd.map ToSend
                )
        ToSend rawMsg ->
            ( gui
            , sendUpdate rawMsg
            )
    --gui |> Tron.applyRaw msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
    -- updateFromDatGui FromDatGui


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port sendUpdate : Exp.RawUpdate -> Cmd msg

port initGui : Exp.RawProperty -> Cmd msg
