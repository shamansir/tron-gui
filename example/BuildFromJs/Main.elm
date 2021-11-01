port module BuildFromJs.Main exposing (..)


import WithTron
import Browser

import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Tree.Expose.Data as Exp


port build : (Exp.Tree -> msg) -> Sub msg

port apply : (List Exp.DeduceIn -> msg) -> Sub msg

port transmit : Exp.Out -> Cmd msg


main : WithTron.Program () () ()
main =
    Browser.element
        <| WithTron.justUiAndCommunication
            (Render.toHtml Dock.middleRight Theme.dark)
            (Communication.receiveJson
                { build = build identity
                , apply = apply identity
                , transmit = transmit
                }
            )
        <| identity
