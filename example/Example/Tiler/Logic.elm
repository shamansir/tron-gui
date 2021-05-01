port module Example.Tiler.Logic exposing (..)


import Html exposing (Html)
import Html.Attributes as HA

import WithTron.ValueAt exposing (ValueAt)


type Msg
    = NoOp
    | TilesetReady String


type alias Model
    = List String


init : flags -> ValueAt -> ( Model, Cmd Msg )
init _ _ = ( [], Cmd.none )


update : Msg -> ValueAt -> Model -> ( Model, Cmd Msg )
update msg _ model =
    case msg of
        TilesetReady tileset ->
            ( tileset :: model, Cmd.none )
        NoOp ->
            ( model, Cmd.none )


view : ValueAt -> Model -> Html Msg
view _ tilesets =
    Html.div []
        <| List.map
            (Html.span [ HA.style "margin" "5px" ]
                << List.singleton
                << Html.text)
        <| tilesets


subscriptions : ValueAt -> Model -> Sub Msg
subscriptions _ _ =
    tilesetReady TilesetReady


port tilesetReady : (String -> msg) -> Sub msg
