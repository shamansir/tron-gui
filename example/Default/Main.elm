module Default.Main exposing (view, update)


import Browser
import Color
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html exposing (Html)
import Html.Attributes as Html

import Gui.Build as Gui
import Gui.Gui exposing (Gui)

import Default.Gui as Simple
import Default.Model exposing (..)
import Default.Model as Model exposing (init)
import Default.Msg exposing (..)


update : Msg -> Model -> Model
update msg model =
    case msg of
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




