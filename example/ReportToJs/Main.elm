port module ReportToJs.Main exposing (..)


import WithTron
import Browser

import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Tree.Expose.Data as Exp


import Example.Unit.Gui as ExampleGui


port ack : Exp.Tree -> Cmd msg

port transmit : Exp.Out -> Cmd msg


main : WithTron.Program () () ()
main =
    Browser.element
        <| WithTron.justUiAndCommunication
            (Render.toHtml Dock.middleRight Theme.dark)
            (Communication.sendJson { ack = ack, transmit = transmit })
        <| always ExampleGui.gui
