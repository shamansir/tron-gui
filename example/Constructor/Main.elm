module Constructor.Main exposing (..)


import Browser exposing (Document)

import Html


type alias Model = ()


type alias Msg = ()


init : () -> ( Model, Cmd Msg )
init _ = ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model = ( model, Cmd.none )


view : Model -> Document Msg
view _ =
    { title = ""
    , body = []
    }


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }