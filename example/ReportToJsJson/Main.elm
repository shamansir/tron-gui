port module ReportToJsJson.Main exposing (main)


import Browser
import Html exposing (Html)

import Gui as Tron
import Gui.Style.Theme as Theme
import Gui.Style.Dock as Dock
import Gui.Expose as Exp
import Gui.Property as Property
import Gui.WithGui as WithGui
import Gui.WithGui exposing (ProgramWithGui)
import Gui.Option as Option exposing (..)

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


{-
type Msg
    = ToTron Tron.Msg
    | ToSend Exp.RawUpdate


type alias Model =
    Tron.Gui Exp.RawUpdate


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Example.init
        ( gui, guiEffect ) =
            ExampleGui.for example
                |> Exp.toExposed
                |> Property.map Tuple.first
                |> Tron.init
    in
        ( gui |> Tron.dock Dock.topRight
        , Cmd.batch
            [ guiEffect
                |> Cmd.map ToTron
            , gui
                |> Tron.encode
                |> initGui
            ]
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


subscriptions : Model -> Sub Msg
subscriptions gui =
    Tron.subscriptions gui
        |> Sub.map ToTron

-}


main : ProgramWithGui () Example.Model Example.Msg
main =
    WithGui.element
        { options =
            [ Option.sendJson
                { ack = initGui
                , transmit = sendUpdate
                }
            , Option.appearance Dock.middleRight Theme.dark
            ]
        , for = ExampleGui.for
        , init = always ( Example.init, Cmd.none )
        , view = always <| Html.div [] [] -- Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }


port sendUpdate : Exp.RawUpdate -> Cmd msg

port initGui : Exp.RawProperty -> Cmd msg
