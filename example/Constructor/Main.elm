module Constructor.Main exposing (..)


import Browser exposing (Document)

import Tron.OfValue as Tron exposing (Tron)
import Tron.Builder as Tron
import Tron.Option as Option
import Tron.Style.Dock as Dock
import Tron.Style.Theme as Theme

import Tron.Path as Path exposing (Path)

import WithTron exposing (ProgramWithTron)

import Html exposing (Html)


type alias Model =
    ( Path
    , Tron ()
    )


type alias Msg = ()


for : Model -> Tron Msg
for = Tuple.second >> Tron.toUnit


init : Model
init = ( Path.start, Tron.root [] )


update : Msg -> Model -> Model
update _ model = model


view : Model -> Html Msg
view _ =
    Html.div [] []


-- subscriptions : Model -> Sub Msg
-- subscriptions _ = Sub.none


main : ProgramWithTron () Model Msg
main =
    WithTron.sandbox
        (Option.toHtml Dock.bottomCenter Theme.dark)
        Option.noCommunication
        { for = for
        , init = init
        , view = view
        , update = update
        --, subscriptions = subscriptions
        }