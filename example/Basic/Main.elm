module Basic.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Gui.Gui as Tron exposing (Gui, init, view, update, subscribe)
import Gui.Msg as Tron exposing (Msg(..))
import Gui.Render.Style as Tron exposing (Theme(..))

import Default.Main as Default
import Default.Model as Default
import Default.Msg as Default
import Default.Gui as DefaultGui


type Msg
    = ToDefault Default.Msg
    | TronUpdate Tron.Msg


type alias Example
    = Default.Model


type alias Model =
    ( Example, Tron.Gui Msg )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Default.init
        ( gui, guiEffect ) =
            DefaultGui.for example
                |> Tron.init Tron.TopToBottom
    in
        (
            ( example
            , gui |> Tron.map ToDefault
            )
        , guiEffect |> Cmd.map TronUpdate
        )


view : Model -> Html Msg
view ( example, gui ) =
    Html.div
        [ ]
        [ gui
            |> Tron.view Tron.Light
            |> Html.map TronUpdate
        , Default.view example
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( example, gui ) =
    case msg of

        ToDefault dmsg ->
            (
                ( example |> Default.update dmsg
                , gui
                )
            , Cmd.none
            )

        TronUpdate guiMsg ->
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
    Tron.subscribe gui |> Sub.map TronUpdate


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
