module OneKnob.Main exposing (main)


import Html exposing (Html)

import Tron exposing (Tron)
import WithTron as WithTron
--import WithTron.ValueAt exposing (ValueAt)
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Build as Builder
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock


type alias Amount = Float


type Msg
    = AmountChanged Amount


type alias Model = Amount


init : Model
init = 0


for : Model -> Tron Msg
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


view : Model -> Html Msg
view amount =
    Html.text
        <| String.fromFloat amount


update : Msg -> Model -> Model
update msg curAmount =
    case msg of

        AmountChanged newAmount ->
            newAmount


main : WithTron.Program () Model Msg
main =
    WithTron.sandbox
        (Render.toHtml Dock.center Theme.dark)
        Communication.none
        { for = for
        , init = init
        , view = view
        , update = update
        }
