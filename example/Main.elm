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
import Task as Task

import Gui.Gui exposing (Gui)
import Gui.Gui as Gui exposing (view)
import Gui.Expose as Exp exposing (Update)
import Gui.Gui as Tron exposing (Gui, focus, over)
import Gui.Msg as Tron exposing (Msg)
import Gui.Mouse exposing (Position)
import Gui.Mouse as Tron exposing (MouseState)

import Simple.Main as Simple
import Simple.Model as Simple
import Simple.Msg as Simple
import Simple.Gui as SimpleGui
import Dict exposing (size)


type Msg
    = ChangeMode Mode
    | DatGuiUpdate Exp.Update
    | TronUpdate Tron.Msg
    | ToSimple Simple.Msg
    | Resize Int Int
    | NoOp


type Example
    = Simple Simple.Model
    | Elmsfeuer


type Mode
    = DatGui
    | TronGui


type alias Model =
    { mode : Mode
    , gui : Tron.Gui Msg
    , example : Example
    , size : ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    update
        (ChangeMode DatGui)
        { mode = DatGui
        , example = Simple <| Simple.init
        , gui = Gui.none
        , size = ( 0, 0 )
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
                    |> Html.map TronUpdate
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
                |> Exp.encode
                |> startDatGui
            )

        ( ChangeMode TronGui, _, Simple simpleExample ) ->
            (
                { model
                | mode = TronGui
                , gui = SimpleGui.for simpleExample
                            |> Gui.over
                            |> Gui.map ToSimple
                }
            , Cmd.batch
                [ destroyDatGui ()
                , Tron.focus NoOp
                , Tron.fromWindow Resize
                ]
            )

        ( ChangeMode TronGui, _, _ ) ->
            (
                { model
                | mode = TronGui
                -- FIXME: build Gui for Elmsfeuer as well
                }
            , Cmd.batch
                [ destroyDatGui ()
                , Tron.focus NoOp
                , Tron.fromWindow Resize
                ]
            )

        ( ToSimple smsg, _, Simple smodel ) ->
            (
                { model
                | example = Simple <| Simple.update smsg smodel
                }
            , Cmd.none
            )

        ( TronUpdate guiMsg, TronGui, Simple simpleExample ) ->
            case gui |> Gui.update guiMsg of
                ( nextGui, cmds ) ->
                    (
                        { model
                        | gui = nextGui
                        }
                    , cmds
                    )

        ( DatGuiUpdate guiUpdate, DatGui, Simple simpleExample ) ->
            ( model
            , model.gui
                |> .tree -- FIXME
                |> Exp.update guiUpdate
            )

        ( Resize width height, _, _ ) ->
            (
                { model
                | size = ( width, height )
                }
            , Cmd.none
            )

        _ -> ( model, Cmd.none )


-- tronGuiFor : Simple.Model -> Gui.Model Simple.Msg
-- tronGuiFor simpleExample =
--     SimpleGui.for simpleExample
--         |> Gui.fromAlt
--         |> Gui.build


subscriptions : Model -> Sub Msg
subscriptions { mode, example, gui, size } =
    case mode of
        DatGui -> updateFromDatGui (DatGuiUpdate << Exp.fromPort)
        TronGui ->
            case example of
                Simple simpleExample ->
                    Sub.batch
                        [ Gui.trackMouse
                            { width = Tuple.first size
                            , height = Tuple.second size
                            }
                            gui.layout -- FIXME
                                |> Sub.map TronUpdate
                        , Browser.onResize Resize
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


port updateFromDatGui : (Exp.PortUpdate -> msg) -> Sub msg

port startDatGui : Encode.Value -> Cmd msg

port destroyDatGui : () -> Cmd msg

port updateDatGui : Encode.Value -> Cmd msg
