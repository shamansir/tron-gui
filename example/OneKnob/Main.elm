module OneKnob.Main exposing (main)


import Html exposing (Html)

import WithTron exposing (ProgramWithTron)
import Tron.Option as Option
import Tron.Builder as Builder exposing (Builder)
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock exposing (Dock(..))


type alias Amount = Float


type Msg
    = AmountChanged Amount


type alias Model = Amount


for : Model -> Builder Msg
for amount =
    Builder.root
        [
            ( "amount"
            , Builder.float
                { min = 0, max = 1, step = 0.01 }
                amount
                AmountChanged
            )
        ]


init : flags -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


view : Model -> Html Msg
view amount =
    Html.text
        <| String.fromFloat amount


update : Msg -> Model -> ( Model, Cmd Msg )
update msg curAmount =
    case msg of

        AmountChanged newAmount ->
            ( newAmount
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _=
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
