module Example.Unit.Main exposing (init, view, update, subscriptions)


import Browser
import Color
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html exposing (Html)
import Html.Attributes as Html

import Gui exposing (Gui)
import Gui.Build as Gui

import Example.Unit.Gui
import Example.Unit.Model exposing (..)
import Example.Unit.Model as Model exposing (default)
import Example.Unit.Msg exposing (..)


init : ( Model, Cmd Msg )
init =
    ( Model.default
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div [] []


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
