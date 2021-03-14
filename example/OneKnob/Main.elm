module OneKnob.Main exposing (main)


import Html exposing (Html)

import Gui.WithGui as WithGui exposing (ProgramWithGui)
import Gui.Option as Option
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock as Dock exposing (Dock(..))


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


main : ProgramWithGui () Model Msg
main =
    WithGui.element
        [ Option.appearance Dock.middleLeft Theme.dark
        ]
        { for = for
        , init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
