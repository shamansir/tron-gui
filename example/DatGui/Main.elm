port module DatGui.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)

import Gui as Tron exposing (Gui, initRaw, view)
import Gui.Expose as Exp exposing (RawProperty, RawUpdate)

import Default.Main as Default
import Default.Model as Default
import Default.Msg as Default
import Default.Gui as DefaultGui


type Msg
    = ToExample Default.Msg
    | FromDatGui Exp.RawUpdate


type alias Example
    = Default.Model


type alias Model =
    ( Example, Tron.Gui Msg )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Default.init
        gui =
            DefaultGui.for example
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
view ( example, _ ) =
    Html.div
        [ ]
        [ Default.view example
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( example, gui ) =
    case msg of

        ToExample dmsg ->
            (
                ( example |> Default.update dmsg
                , gui
                )
            , Cmd.none
            )

        FromDatGui datGuiMsg ->
            (
                ( example
                , gui
                )
            , gui |> Tron.applyRaw datGuiMsg
            )


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


port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port startDatGui : Exp.RawProperty -> Cmd msg
