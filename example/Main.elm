port module Main exposing (main)


import Browser
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (Html)
import Html as Html exposing (map, div)
import Html.Events as Html exposing (onClick)

import Gui.Alt as AGui
import Gui.Alt exposing (Gui)
import Gui.Gui as Gui exposing (view, fromAlt)
import Gui.Msg as Tron exposing (Msg)

import Simple.Main as Simple
import Simple.Model as Simple
import Simple.Msg as Simple
import Simple.Gui as SimpleGui


type Msg
    = ChangeMode Mode
    | DatGuiUpdate AGui.Update
    | TronSUpdate (Tron.Msg Simple.Msg)
    | ToSimple Simple.Msg
    | NoOp


type Example
    = Simple Simple.Model
    | Elmsfeuer


type Mode
    = DatGui
    | TronGui


type alias Model =
    ( Mode, Example )


init : ( Model, Cmd Msg )
init =
    update
        (ChangeMode DatGui)
        ( TronGui, Simple <| Simple.init )


view : Model -> Html Msg
view ( mode, example ) =
    Html.div
        [ ]
        [ Html.button
            [ Html.onClick <| ChangeMode TronGui ]
            [ Html.text "Tron" ]
        , Html.button
            [ Html.onClick <| ChangeMode DatGui ]
            [ Html.text "Dat.gui" ]
        , case mode of
            DatGui -> Html.div [] []
            TronGui -> case example of
                Simple simpleExample
                    -> SimpleGui.for simpleExample
                        |> Gui.fromAlt
                        |> Gui.build
                        |> Gui.view
                        |> Html.map TronSUpdate
                _ -> Html.div [] []
        , case example of
            Simple simpleExample ->
                Simple.view simpleExample |> Html.map (always NoOp)
            Elmsfeuer -> Html.div [] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( mode, example ) =
    case ( msg, mode, example ) of
        ( ChangeMode DatGui, _, Simple simpleExample ) ->
            (
                ( DatGui
                , example
                )
            , SimpleGui.for simpleExample
                |> AGui.encode
                |> startDatGui
            )
        ( ChangeMode TronGui, _, _ ) ->
            (
                ( TronGui
                , example
                )
            , destroyDatGui ()
            )
        ( TronSUpdate guiUpdate, TronGui, Simple simpleExample ) ->
            let
                ( ( newModel, commands ), newGui ) =
                    SimpleGui.for simpleExample
                        |> Gui.fromAlt
                        |> Gui.build
                        |> Gui.update
                            (\smsg smodel -> ( Simple.update smsg smodel, Cmd.none ) )
                            simpleExample
                            guiUpdate
            in
                ( ( TronGui, Simple newModel )
                , commands |> Cmd.map ToSimple
                )
        ( DatGuiUpdate guiUpdate, DatGui, Simple simpleExample ) ->
            (
                ( DatGui
                , SimpleGui.for simpleExample
                    |> AGui.update guiUpdate
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
        DatGui -> updateFromDatGui (DatGuiUpdate << AGui.fromPort)
        TronGui -> Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port updateFromDatGui : (AGui.PortUpdate -> msg) -> Sub msg

port startDatGui : Encode.Value -> Cmd msg

port destroyDatGui : () -> Cmd msg

port updateDatGui : Encode.Value -> Cmd msg
