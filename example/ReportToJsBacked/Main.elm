port module ReportToJsBacked.Main exposing (..)


import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option
import Tron.Expose.Data as Exp

import WithTron.Backed exposing (BackedByJson)


import Example.Unit.Gui as ExampleGui


port ack : Exp.Property -> Cmd msg

port transmit : Exp.Out -> Cmd msg


main : BackedByJson
main =
    WithTron.Backed.byJson
        (Option.toHtml Dock.middleRight Theme.dark)
        ( ack, transmit )
        ExampleGui.gui
