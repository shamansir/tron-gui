module Main exposing (main)


import Browser
import Json.Decode as Decode
import Json.Encode as Encode
import Html

import Gui.GuiAlt as Gui
import Gui.GuiAlt exposing (Gui)


type Msg
    = NoOp


gui : Gui Msg
gui = [ ( "root", Gui.ghost ) ]


init = gui


view model =
    Html.div
        []
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
