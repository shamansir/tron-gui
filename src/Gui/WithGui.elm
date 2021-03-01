module Gui.WithGui exposing (..)


import Browser
import Html exposing (Html)
import Either exposing (Either(..))

import Gui as Tron
import Gui.Build as Builder exposing (Builder)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock exposing (Dock(..))


type Option
    = Hidden
    | Theme Theme
    | Dock Dock
    | SendJsonToJs
        {}
    | SendStringsToJs
        {}
    | AFrame
    | Detachable
        {}
    | DatGui
        {}



init
    :  ( flags -> ( model, Cmd msg ), model -> Builder msg )
    -> List Option
    -> flags
    -> ( ( model, Tron.Gui msg ), Cmd (Either msg Tron.Message) )
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
            , gui --|> Tron.dock def.dock
            )
        , Cmd.batch
            [ userEffect |> Cmd.map Left
            , guiEffect |> Cmd.map Right
            ]
        )


view userView options ( model, gui ) =
    Html.div
        [ ]
        [ gui
            |> Tron.view Theme.light -- def.theme
            |> Html.map Right
        , userView model
            |> Html.map Left
        ]


subscriptions
    :  ( model -> Sub msg )
    -> List Option
    -> ( model, Tron.Gui msg )
    -> Sub (Either msg Tron.Message)
subscriptions userSubscriptions options ( model, gui ) =
    Sub.batch
        [ userSubscriptions model |> Sub.map Left
        , Tron.subscriptions gui |> Sub.map Right
        ]


update
    :  ( msg -> model -> (model, Cmd msg), model -> Builder msg )
    -> List Option
    -> Either msg Tron.Message
    -> ( model, Tron.Gui msg )
    -> ( ( model, Tron.Gui msg ), Cmd (Either msg Tron.Message) )
update ( userUpdate, userFor ) options eitherMsg (model, gui) =
    case eitherMsg of

        Left userMsg ->
            let
                ( newUserModel, userEffect ) =
                    userUpdate userMsg model
            in

            (
                ( newUserModel
                , gui
                    |> Tron.over (userFor model)
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



element
    :
        { options : List Option
        , init : flags -> ( model, Cmd msg )
        , for : model -> Builder.Builder msg
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> Program flags ( model, Tron.Gui msg ) (Either msg Tron.Message)
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
