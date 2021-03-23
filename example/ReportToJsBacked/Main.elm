port module ReportToJsBacked.Main exposing (..)


import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option
import WithTron exposing (BackedWithTron)


import Example.Unit.Gui as ExampleGui


port sendUpdate : ( String, String ) -> Cmd msg


main : BackedWithTron
main =
    WithTron.backed
        (Option.toHtml Dock.bottomLeft Theme.dark)
        sendUpdate
        ExampleGui.gui


