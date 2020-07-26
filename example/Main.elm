port module Main exposing (main)


import Browser
import Browser.Events as Browser
import Browser.Dom as Browser
import Json.Decode as Decode
import Json.Decode as D
import Json.Encode as Encode
import Html exposing (Html)
import Html as Html exposing (map, div)
import Html.Events as Html exposing (onClick)

import Gui.Alt as AGui
import Gui.Alt exposing (Gui)
import Gui.Gui as Gui exposing (view, fromAlt)
import Gui.Gui as Tron exposing (Model)
import Gui.Msg as Tron exposing (Msg)
import Gui.Mouse exposing (Position)

import Simple.Main as Simple
import Simple.Model as Simple
import Simple.Msg as Simple
import Simple.Gui as SimpleGui


type Msg
    = ChangeMode Mode
    | DatGuiUpdate AGui.Update
    | TronUpdate Tron.Msg
    | ToSimple Simple.Msg
    | Joined Tron.Msg Simple.Msg
    | NoOp


type Example
    = Simple Simple.Model
    | Elmsfeuer


type Mode
    = DatGui
    | TronGui


type alias Model =
    { mode : Mode
    , gui : Tron.Model Msg
    , example : Example
    }
    -- ( Mode, Example )


init : ( Model, Cmd Msg )
init =
    update
        (ChangeMode DatGui)
        { mode = DatGui
        , example = Simple <| Simple.init
        , gui = Gui.none
        }


view : Model -> Html Msg
view { mode, gui, example } =
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
            TronGui ->
                gui
                    |> Gui.view
                    |> Html.map
                        (\(tronMsg, maybeSimpleMsg) ->
                            case maybeSimpleMsg of
                                Just (ToSimple simpleMsg) ->
                                    Joined tronMsg simpleMsg
                                -- FIXME: the possible messages are not only `toSimple`
                                _ -> TronUpdate tronMsg
                        )
        , case example of
            Simple simpleExample ->
                Simple.view simpleExample |> Html.map (always NoOp)
            Elmsfeuer -> Html.div [] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ mode, example, gui } as model) =
    case ( msg, mode, example ) of
        ( ChangeMode DatGui, _, Simple simpleExample ) ->
            (
                { model
                | mode = DatGui
                -- FIXME: update Gui model as well
                }
            , SimpleGui.for simpleExample
                |> AGui.encode
                |> startDatGui
            )
        ( ChangeMode TronGui, _, Simple simpleExample ) ->
            (
                { model
                | mode = TronGui
                , gui = SimpleGui.for simpleExample
                    |> Gui.fromAlt
                    |> Gui.build
                    |> Gui.map ToSimple
                }
            , destroyDatGui ()
            )
        ( ChangeMode TronGui, _, _ ) ->
            (
                { model
                | mode = TronGui
                -- FIXME: build Gui for Elmsfeuer as well
                }
            , destroyDatGui ()
            )
        ( ToSimple smsg, _, Simple smodel ) ->
            (
                { model
                | example = Simple <| Simple.update smsg smodel
                }
            , Cmd.none
            )
        ( TronUpdate guiMsg, TronGui, Simple simpleExample ) ->
            let
                ( nextGui, maybeUserMsg ) =
                    gui |> Gui.update guiMsg
            in
                -- (\smsg smodel -> ( Simple.update smsg smodel, Cmd.none ) )
                (
                    { model
                    | gui = nextGui
                    , example =
                        case maybeUserMsg of
                            Just (ToSimple simpleMsg) ->
                                Simple <| Simple.update simpleMsg simpleExample
                            Just _ -> example -- FIXME
                            Nothing -> example
                    }
                , Cmd.none
                )
        ( DatGuiUpdate guiUpdate, DatGui, Simple simpleExample ) ->
            (
                { model
                | example =
                    SimpleGui.for simpleExample
                        |> AGui.update guiUpdate
                        |> Maybe.map (\simpleMsg ->
                                Simple.update simpleMsg simpleExample
                            )
                        |> Maybe.withDefault simpleExample
                        |> Simple
                }
            , Cmd.none
            )
        _ -> ( model, Cmd.none )


-- tronGuiFor : Simple.Model -> Gui.Model Simple.Msg
-- tronGuiFor simpleExample =
--     SimpleGui.for simpleExample
--         |> Gui.fromAlt
--         |> Gui.build


decodeMousePosition : D.Decoder Position
decodeMousePosition =
    D.map2 Position
        (D.at [ "offsetX" ] D.int)
        (D.at [ "offsetY" ] D.int)


subscriptions : Model -> Sub Msg
subscriptions { mode, example, gui } =
    case mode of
        DatGui -> updateFromDatGui (DatGuiUpdate << AGui.fromPort)
        TronGui ->
            case example of
                Simple simpleExample ->
                    Sub.batch
                        [ Sub.map (Gui.downs gui >> TronUpdate)
                            <| Browser.onMouseDown
                            <| decodeMousePosition
                        , Sub.map (Gui.ups gui >> TronUpdate)
                            <| Browser.onMouseUp
                            <| decodeMousePosition
                        , Sub.map (Gui.moves gui >> TronUpdate)
                            <| Browser.onMouseMove
                            <| decodeMousePosition
                        ]
                _ -> Sub.none


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
