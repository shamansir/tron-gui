port module Detachable.Main exposing (main)


import Url exposing (Url)

import Browser exposing (element)
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Gui as Tron exposing (Gui, init, view, update, subscriptions)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock as Dock
import Gui.Detach as Detach exposing (fromUrl)
import Gui.Expose as Exp exposing (RawProperty, RawUpdate)
import Gui.Build as Builder exposing (map)
import Gui.WithGui as WithGui exposing (ProgramWithGui)
import Gui.Option as Option


import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


{-
-- Change to `Default` example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui
-}


{- type Msg
    = NoOp
    | ToExample Example.Msg
    | ToTron Tron.Msg


type alias Model =
    ( Example.Model, Tron.Gui Msg )


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url _ =
    let
        example = Example.init
        ( gui, guiEffect ) =
            ExampleGui.for example
                |> Tron.init
        ( nextGui, detachableEffect ) =
            gui
                |> Tron.detachable
                    url
                    ackToWs
                    sendUpdateToWs
                    receieveUpdateFromWs
    in
        (
            ( example
            , nextGui
                |> Tron.map ToExample
            )
        , Cmd.batch
            [ guiEffect
            , detachableEffect
            ]
            |> Cmd.map ToTron
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

        NoOp ->
            ( ( example, gui )
            , Cmd.none
            )

        ToExample dmsg ->
            (
                -- If your GUI structure never changes (unlike with `Goose` example),
                -- you need neither `updatedBy` function nor this condition check,
                -- at all! Just leave the `else` part in your code.
                if ExampleGui.updatedBy dmsg then
                    let nextModel = example |> Example.update dmsg
                    in
                        ( nextModel
                        , gui
                            |> Tron.over
                                (ExampleGui.for nextModel |> Builder.map ToExample)
                        )
                else
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
    Tron.subscriptions gui |> Sub.map ToTron -}


main : ProgramWithGui () Example.Model Example.Msg
main =
    WithGui.application
        (Option.toHtml Dock.center Theme.light)
        (Option.detachable
            { ack = ackToWs
            , transmit = sendUpdateToWs
            , receive = receieveUpdateFromWs
            }
        )
        { for = ExampleGui.for
        , init = always Example.init
        , view = Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }


port receieveUpdateFromWs : (Exp.RawUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawUpdate -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
