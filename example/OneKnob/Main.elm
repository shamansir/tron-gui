module OneKnob.Main exposing (main)


import Browser
import Html exposing (Html)

import Gui as Tron
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))


type alias Amount = Float


type Msg
    = AmountChanged Amount
    | ToTron Tron.Msg


type alias Model =
    ( Amount
    , Tron.Gui Msg
    )


for : Amount -> Builder Msg
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
    let
        initialAmount = 0
        ( gui, guiEffect ) =
            for initialAmount
                |> Tron.init
    in
        (
            ( initialAmount
            , gui
            )
        , guiEffect |> Cmd.map ToTron
        )


view : Model -> Html Msg
view ( amount, gui ) =
    Html.div
        [ ]
        [ gui
            |> Tron.view Theme.dark
            |> Html.map ToTron
        , Html.text
            <| String.fromFloat amount
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( curAmount, gui ) =
    case msg of

        AmountChanged newAmount ->
            (
                ( newAmount
                , gui
                )
            , Cmd.none
            )

        ToTron guiMsg ->
            case gui |> Tron.update guiMsg of
                ( nextGui, commands ) ->
                    (
                        ( curAmount
                        , nextGui
                        )
                    , commands
                    )


subscriptions : Model -> Sub Msg
subscriptions ( _, gui ) =
    Tron.subscriptions gui
        |> Sub.map ToTron


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
