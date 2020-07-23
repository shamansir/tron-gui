module Main exposing (main)


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


type Msg
    = SimpleMsg Simple.Msg
    | ElmsfeuerMsg


type Example
    = Simple Simple.Model
    | Elmsfeuer
    | None


init : Example
init = Simple Simple.init


view : Example -> Html Msg
view example =
    case example of
        Simple simpleExample ->
            Simple.view simpleExample |> Html.map SimpleMsg
        Elmsfeuer -> Html.div [] []
        None -> Html.div [] []


update : Msg -> Example -> Example
update msg example =
    case ( msg, example ) of
        ( SimpleMsg simpleMsg, Simple simpleExample ) ->
            Simple <| Simple.update simpleMsg simpleExample
        ( ElmsfeuerMsg, Elmsfeuer ) ->
            Elmsfeuer
        _ -> example


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
