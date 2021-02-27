port module ReportToJsJson.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)

import Gui as Tron exposing (Gui, initRaw, view)
import Gui.Expose as Exp exposing (RawProperty, RawUpdate)

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


type alias Msg
    = Tron.Message


type alias Model =
    Tron.Gui Exp.RawUpdate


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Example.init
        gui =
            ExampleGui.for example
                |> Tron.initRaw
    in
        (
            ( example
            , gui |> Tron.map ToExample
            )
        , gui
            |> Tron.encode
            |> startDatGui
        )


view : Model -> Html Msg
view gui =
    Html.div
        [ ]
        [ Tron.view gui
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg gui =
    gui |> Tron.applyRaw msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    updateFromDatGui FromDatGui


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port in : (Exp.RawUpdate -> msg) -> Sub msg

port out : Exp.RawUpdate -> Cmd msg

port init : Exp.RawProperty -> Cmd msg
