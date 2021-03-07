module Gui.WithGui exposing (..)


import Browser
import Html exposing (Html)
import Either exposing (Either(..))

import Gui as Tron
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Option exposing (..)
import Gui.Msg exposing (Msg_(..))


type WithGuiMsg msg
    = ToUser msg
    | ToTron Tron.Msg


init
    :  ( flags -> ( model, Cmd msg ), model -> Builder msg )
    -> List (Option msg)
    -> flags
    -> ( ( model, Tron.Gui msg ), Cmd (WithGuiMsg msg) )
init ( userInit, userFor ) options flags =
    let
        ( initialModel, userEffect ) =
            userInit flags
        ( gui, guiEffect ) =
            userFor initialModel
                |> Tron.init

    in
        (
            ( initialModel
            , gui |> addInitOptions options
            )
        , Cmd.batch
            [ userEffect |> Cmd.map ToUser
            , guiEffect |> Cmd.map ToTron
            , performInitEffects options gui |> Cmd.map ToUser
            ]
        )


view
    :  (model -> Html msg)
    -> List (Option msg)
    -> (model, Tron.Gui msg)
    -> Html (WithGuiMsg msg)
view userView options ( model, gui ) =
    Html.div
        [ ]
        [ gui
            |> addViewOptions options
            |> Html.map ToTron
        , userView model
            |> Html.map ToUser
        ]


subscriptions
    :  ( model -> Sub msg )
    -> List (Option msg)
    -> ( model, Tron.Gui msg )
    -> Sub (WithGuiMsg msg)
subscriptions userSubscriptions options ( model, gui ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map ToUser
        , Tron.subscriptions gui |> Sub.map ToTron
        , addSubscriptionsOptions options gui |> Sub.map ToUser
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Builder msg )
    -> List (Option msg)
    -> WithGuiMsg msg
    -> ( model, Tron.Gui msg )
    -> ( ( model, Tron.Gui msg ), Cmd (WithGuiMsg msg) )
update ( userUpdate, userFor ) options eitherMsg (model, gui) =
    case eitherMsg of

        ToUser userMsg ->
            let
                ( newUserModel, userEffect ) =
                    userUpdate userMsg model
            in

            (
                ( newUserModel
                , gui
                    |> Tron.over (userFor model)
                )
            , userEffect |> Cmd.map ToUser
            )

        ToTron guiMsg ->
            case gui |> Tron.update guiMsg of
                ( nextGui, guiEffect ) ->
                    (
                        ( model
                        , nextGui
                        )
                    , Cmd.batch
                        [ guiEffect
                            |> Cmd.map ToUser
                        , gui
                            |> performUpdateEffects options guiMsg
                            |> Cmd.map ToUser
                        ]
                    )


addInitOptions : List (Option msg) -> Tron.Gui msg -> Tron.Gui msg
addInitOptions options gui =
    options
        |> List.foldl
            (\option gui_ ->
                case option of
                    Dock target ->
                        gui_ |> Tron.dock target
                    _ ->
                        gui_
            )
            gui


performInitEffects : List (Option msg) -> Tron.Gui msg -> Cmd msg
performInitEffects options gui =
    options
        |> List.foldl
            (\option cmds ->
                case option of
                    SendJsonToJs { ack } ->
                        (gui
                            |> Tron.encode
                            |> ack
                        ) :: cmds
                    _ ->
                        cmds
            )
            []
        |> Cmd.batch


performUpdateEffects : List (Option msg) -> Tron.Msg -> Tron.Gui msg -> Cmd msg
performUpdateEffects options msg gui =
    options
        |> List.foldl
            (\option cmds ->
                case option of
                    SendJsonToJs { ack } ->
                        (gui.tree
                            |> Exp.toExposed
                            |> Tron.over gui
                            |> Tron.update msg
                            |> ack
                        ) :: cmds
                    _ ->
                        cmds
            )
            []
        |> Cmd.batch


addSubscriptionsOptions : List (Option msg) -> Tron.Gui msg -> Sub msg
addSubscriptionsOptions options gui =
    Sub.none


addViewOptions : List (Option msg) -> Tron.Gui msg -> Html Tron.Msg
addViewOptions options gui =
    options
        |> List.foldl
            (\option _ ->
                case option of
                    Hidden ->
                        []
                    Theme theme ->
                        [ gui |> Tron.view theme ]
                    -- FIXME: AFrame
                    _ ->
                        []
            )
            []
        |> Html.div []


element
    :
        { options : List (Option msg)
        , init : flags -> ( model, Cmd msg )
        , for : model -> Builder.Builder msg
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> Program flags ( model, Tron.Gui msg ) (WithGuiMsg msg)
element def =
    Browser.element
        { init =
            init ( def.init, def.for ) def.options
        , view =
            view def.view def.options
        , subscriptions =
            subscriptions def.subscriptions def.options
        , update =
            update ( def.update, def.for ) def.options
        }
