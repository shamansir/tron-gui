module Basic.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Gui as Tron exposing (Gui, init, view, update, subscriptions)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Style.Theme as Theme exposing (Theme(..))

import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui


type Msg
    = ToExample Example.Msg
    | ToTron Tron.Msg


type alias Model =
    ( Example.Model, Tron.Gui Msg )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Example.init
        ( gui, guiEffect ) =
            ExampleGui.for example
                |> Tron.init
    in
        (
            ( example
            , gui |> Tron.map ToExample
            )
        , guiEffect |> Cmd.map ToTron
        )


view : Model -> Html Msg
view ( example, gui ) =
    Html.div
        [ ]
        [ gui
            |> Tron.view Theme.light
            |> Html.map ToTron
        , Example.view example
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( example, gui ) =
    case msg of

        ToExample dmsg ->
            (
                ( example |> Example.update dmsg
                , gui
                )
            , Cmd.none
            )

        ToTron guiMsg ->
            case gui |> Tron.update guiMsg of
                ( nextGui, cmds ) ->
                    (
                        ( example
                        , nextGui
                        )
                    , cmds
                    )


subscriptions : Model -> Sub Msg
subscriptions ( _, gui ) =
    Tron.subscriptions gui |> Sub.map ToTron


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
