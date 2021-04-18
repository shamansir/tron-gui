module Example.Default.Main exposing (init, view, update, subscriptions)


import Browser
import Color
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html exposing (Html)
import Html.Attributes as Html

import Tron exposing (Tron)
import Tron.Builder as Gui

import Example.Default.Gui
import Example.Default.Model exposing (..)
import Example.Default.Model as Model
import Example.Default.Msg exposing (..)


init : ( Model, Cmd Msg )
init =
    ( Model.default
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        NoOp -> model
        ChangeInt int -> { model | int = int }
        ChangeFloat float -> { model | float = float }
        ChangeString string -> { model | string = string }
        ChangeColor color -> { model | color = color }
        ChangeRed red ->
            { model | color =
                case model.color |> Color.toRgba of
                    { green, blue, alpha } ->
                        Color.fromRgba
                            { red = red / 255
                            , green = green
                            , blue = blue
                            , alpha = alpha
                            }
            }
        ChangeGreen green ->
            { model | color =
                case model.color |> Color.toRgba of
                    { red, blue, alpha } ->
                        Color.fromRgba
                            { red = red
                            , green = green / 255
                            , blue = blue
                            , alpha = alpha
                            }
            }
        ChangeBlue blue ->
            { model | color =
                case model.color |> Color.toRgba of
                    { red, green, alpha } ->
                        Color.fromRgba
                            { red = red
                            , green = green
                            , blue = blue / 255
                            , alpha = alpha
                            }
            }
        ChangeXY xy -> { model | xy = xy }
        Choose choice -> { model | choice = choice }
        Switch to -> { model | toggle = to }
        Pressed choice -> { model | buttonPressed = choice }
    , Cmd.none
    )


view : Model -> Html msg
view model =
    Html.ul
        []
        [ Html.li [] [ Html.text <| String.fromInt model.int ]
        , Html.li [] [ Html.text <| String.fromFloat model.float ]
        , Html.li [] [ Html.text <|
                ( String.fromFloat <| Tuple.first model.xy ) ++ ":" ++
                ( String.fromFloat <| Tuple.second model.xy )
            ]
        , Html.li [] [ Html.text <| model.string ]
        , Html.li [] [ Html.text <| choiceToString model.choice ]
        , Html.li
            [ Html.style "background-color" <| Color.toCssString model.color ]
            [ Html.text <| Color.toCssString model.color ]
        , Html.li [] [ Html.text <| if model.toggle then "On" else "Off" ]
        , Html.li [] [ Html.text <| choiceToString model.buttonPressed ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
