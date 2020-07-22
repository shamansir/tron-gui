module Main exposing (main)


import Browser
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html.Attributes as Html

import Gui.GuiAlt as Gui
import Gui.GuiAlt exposing (Gui)


type Msg
    = NoOp


gui : Gui Msg
gui = [ ( "root", Gui.ghost ) ]


init = gui


view model =
    Html.textarea
        [ Html.style "border" "0px"
        , Html.style "width" "100%"
        , Html.style "height" "100vh"
        , Html.style "overflow" "hidden"
        , Html.style "outline" "none"
        , Html.style "resize" "none"
        ]
        [ Html.text
            <| Encode.encode 4
            <| Gui.encode init
        ]


update msg model = model


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
