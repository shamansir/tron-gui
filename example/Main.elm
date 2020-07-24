port module Main exposing (main)


import Browser
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (Html)
import Html as Html exposing (map, div)

import Gui.GuiAlt as Gui
import Gui.GuiAlt exposing (Gui)

import Simple.Main as Simple
import Simple.Model as Simple
import Simple.Msg as Simple
import Simple.Gui as SimpleGui


type Msg
    = ChangeMode Mode
    | DatGuiUpdate Gui.Update
    | NoOp


type Example
    = Simple Simple.Model
    | Elmsfeuer


type Mode
    = DatGui
    | Tron


type alias Model =
    ( Mode, Example )


init : ( Model, Cmd Msg )
init =
    ( ( DatGui, Simple.init |> Simple )
    , SimpleGui.for Simple.init
        |> Gui.encode
        |> startDatGui
    )


view : Model -> Html Msg
view ( mode, example )=
    case example of
        Simple simpleExample ->
            Simple.view simpleExample |> Html.map (always NoOp)
        Elmsfeuer -> Html.div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( mode, example ) =
    case ( mode, msg, example ) of
        ( DatGui, DatGuiUpdate guiUpdate, Simple simpleExample ) ->
            (
                ( mode
                , SimpleGui.for simpleExample
                    |> Gui.update guiUpdate
                    |> Maybe.map (\simpleMsg ->
                            Simple.update simpleMsg simpleExample
                        )
                    |> Maybe.withDefault simpleExample
                    |> Simple
                )
            , Cmd.none
            )
        _ -> ( ( mode, example ), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions ( mode, _ ) =
    case mode of
        DatGui -> updateFromDatGui (DatGuiUpdate << Gui.fromPort)
        Tron -> Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port updateFromDatGui : (Gui.PortUpdate -> msg) -> Sub msg

port startDatGui : Encode.Value -> Cmd msg

port destroyDatGui : Encode.Value -> Cmd msg

port updateDatGui : Encode.Value -> Cmd msg
