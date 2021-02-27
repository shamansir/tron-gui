module Gui.WithGui exposing (..)


import Browser
import Html exposing (Html)
import Either exposing (Either(..))

import Gui as Tron
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))


{-
type alias Amount = Float


type WithGuiMsg msg
    = ToApp msg
    | ToTron Tron.Message


type alias WithGuiModel model msg =
    ( model
    , Tron.Gui msg
    )


init : flags -> model -> ( WithGuiModel model msg, Cmd (WithGuiMsg msg) )
init flags initalModel =
    let
        ( gui, guiEffect ) =
            for initialModel
                |> Tron.init
    in
        (
            ( initialAmount
            , gui
            )
        , guiEffect |> Cmd.map ToTron
        )


view : Model -> Html (WithGuiMsg msg)
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
-}


element
    :
        { init : flags -> ( model, Cmd msg )
        , for : model -> Builder.Builder msg
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> Program flags ( model, Tron.Gui msg ) (Either msg Tron.Message)
element def =
    Browser.element
        { init =
            \flags ->
                let
                    ( initialModel, userEffect ) =
                        def.init flags
                    ( gui, guiEffect ) =
                        def.for initialModel
                            |> Tron.init
                in
                    (
                        ( initialModel
                        , gui
                        )
                    , Cmd.batch
                        [ userEffect |> Cmd.map Left
                        , guiEffect |> Cmd.map Right
                        ]
                    )
        , view =
            \(model, gui) ->
                Html.div
                    [ ]
                    [ gui
                        |> Tron.view Theme.dark
                        |> Html.map Right
                    , def.view model
                        |> Html.map Left
                    ]
        , subscriptions =
            \( model, gui ) ->
                Sub.batch
                    [ def.subscriptions model |> Sub.map Left
                    , Tron.subscriptions gui |> Sub.map Right
                    ]
        , update =
            \eitherMsg (model, gui) ->
                case eitherMsg of

                    Left userMsg ->
                        let
                            ( newUserModel, userEffect ) =
                                def.update userMsg model
                        in

                        (
                            ( newUserModel
                            , gui
                                |> Tron.over (def.for model)
                            )
                        , userEffect |> Cmd.map Left
                        )

                    Right guiMsg ->
                        case gui |> Tron.update guiMsg of
                            ( nextGui, guiEffect ) ->
                                (
                                    ( model
                                    , nextGui
                                    )
                                , guiEffect
                                    |> Cmd.map Left
                                )
        }
