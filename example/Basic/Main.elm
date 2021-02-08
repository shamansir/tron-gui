module Basic.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Gui as Tron exposing (Gui, Message, init, view, update, subscriptions)
import Gui.Build as Builder exposing (map)
import Gui.Style.Theme as Theme exposing (Theme(..))


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
    | ToTron Tron.Message


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
    Tron.subscriptions gui |> Sub.map ToTron


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
