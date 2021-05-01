port module ForTiler.Main exposing (..)


import Html exposing (Html)
import Html.Attributes as HA

import Tron
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option
import Tron.Expose.Data as Exp

import WithTron.Backed exposing (AppBackedByProxy)
import WithTron.ValueAt exposing (ValueAt)


import Example.Tiler.Gui as ExampleGui


main : AppBackedByProxy () Model Msg
main =
    WithTron.Backed.byProxyApp
        (Option.toHtml Dock.bottomCenter Theme.dark)
        ( ack, transmit )
        { for =
            \valueAt model ->
                ExampleGui.gui valueAt
                    |> Tron.map ( always NoOp )
        , init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


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


port ack : Exp.RawProperty -> Cmd msg

port transmit : Exp.RawOutUpdate -> Cmd msg


port tilesetReady : (String -> msg) -> Sub msg





