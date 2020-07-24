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
    = GuiUpdate Gui.Update
    | SimpleMsg Simple.Msg
    | ElmsfeuerMsg


type Example
    = Simple Simple.Model
    | Elmsfeuer
    | None


init : ( Example, Cmd Msg )
init =
    ( Simple.init |> Simple
    , SimpleGui.for Simple.init
        |> Gui.encode
        |> startGui
    )


view : Example -> Html Msg
view example =
    case example of
        Simple simpleExample ->
            Simple.view simpleExample |> Html.map SimpleMsg
        Elmsfeuer -> Html.div [] []
        None -> Html.div [] []


update : Msg -> Example -> ( Example, Cmd Msg )
update msg example =
    case ( msg, example ) of
        ( GuiUpdate guiUpdate, Simple simpleExample ) ->
            SimpleGui.for simpleExample
                |> Gui.update guiUpdate
                |> Maybe.map (\simpleMsg -> update (SimpleMsg simpleMsg) <| Simple simpleExample)
                |> Maybe.withDefault ( Simple simpleExample, Cmd.none )
        ( SimpleMsg simpleMsg, Simple simpleExample ) ->
            ( Simple <| Simple.update simpleMsg simpleExample, Cmd.none )
        ( ElmsfeuerMsg, Elmsfeuer ) ->
            ( Elmsfeuer, Cmd.none )
        _ -> ( example, Cmd.none )


subscriptions : Example -> Sub Msg
subscriptions example =
    valueUpdate (GuiUpdate << Gui.fromPort)


main : Program () Example Msg
main =
    Browser.element
        { init = always init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port valueUpdate : (Gui.PortUpdate -> msg) -> Sub msg

port startGui : Encode.Value -> Cmd msg

port updateGui : Encode.Value -> Cmd msg
