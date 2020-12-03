port module DatGui.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)

import Gui as Tron exposing (Gui, initRaw, view)
import Gui.Expose as Exp exposing (RawProperty, RawUpdate)

import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


{-
-- Change to a more boring example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui
-}


type Msg
    = ToExample Example.Msg
    | FromDatGui Exp.RawUpdate


type alias Example
    = Example.Model


type alias Model =
    ( Example, Tron.Gui Msg )


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        example = Example.init
        gui =
            ExampleGui.for example
                |> Tron.initRaw
    in
        (
            ( example
            , gui |> Tron.map ToExample
            )
        , gui
            |> Tron.encode
            |> startDatGui
        )


view : Model -> Html Msg
view ( example, _ ) =
    Html.div
        [ ]
        [ Example.view example
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

        FromDatGui datGuiMsg ->
            (
                ( example
                , gui
                )
            , gui |> Tron.applyRaw datGuiMsg
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    updateFromDatGui FromDatGui


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port startDatGui : Exp.RawProperty -> Cmd msg
