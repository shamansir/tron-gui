port module ForTiler.Main exposing (..)


import Html exposing (Html)

import Tron
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option
import Tron.Expose.Data as Exp

import WithTron.Backed exposing (ValueAt, AppBackedByProxy)


import Example.Tiler.Gui as ExampleGui


type Msg =
    NoOp


type alias Model
    = List String


init : flags -> ValueAt -> ( Model, Cmd Msg )
init _ _ = ( [], Cmd.none )


update : Msg -> ValueAt -> Model -> ( Model, Cmd Msg )
update _ _ model = ( model, Cmd.none )


view : ValueAt -> Model -> Html Msg
view _ _ = Html.div [] []


subscriptions : ValueAt -> Model -> Sub Msg
subscriptions _ _ = Sub.none


port ack : Exp.RawProperty -> Cmd msg

port transmit : Exp.RawOutUpdate -> Cmd msg


main : AppBackedByProxy () Model Msg
main =
    WithTron.Backed.byProxyApp
        (Option.toHtml Dock.bottomCenter Theme.dark)
        ( ack, transmit )
        { for = \valueAt model -> ExampleGui.gui valueAt |> Tron.map (always NoOp )
        , init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


