module OneKnob.Main exposing (main)


import Html exposing (Html)

import Tron exposing (Tron)
import WithTron exposing (ProgramWithTron)
import WithTron.ValueAt exposing (ValueAt)
import Tron.Option.Render as Option
import Tron.Option.Communication as Option
import Tron.Build as Builder
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock


type alias Amount = Float


type Msg
    = AmountChanged Amount


type alias Model = Amount


init : flags -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


for : ValueAt -> Model -> Tron Msg
for _ amount =
    Builder.root
        [
            ( "amount"
            , Builder.float
                { min = 0, max = 1, step = 0.01 }
                amount
                AmountChanged
            )
        ]


view : ValueAt -> Model -> Html Msg
view _ amount =
    Html.text
        <| String.fromFloat amount


update : Msg -> ValueAt -> Model -> ( Model, Cmd Msg )
update msg _ curAmount =
    case msg of

        AmountChanged newAmount ->
            ( newAmount
            , Cmd.none
            )


subscriptions : ValueAt -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


main : ProgramWithTron () Model Msg
main =
    WithTron.element
        (Option.toHtml Dock.center Theme.dark)
        Option.noCommunication
        { for = for
        , init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
