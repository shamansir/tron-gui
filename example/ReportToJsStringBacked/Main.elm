port module ReportToJsStringBacked.Main exposing (..)


import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option

import WithTron.Backed exposing (BackedByStrings)


import Example.Unit.Gui as ExampleGui


port sendUpdate : ( List String, String ) -> Cmd msg


main : BackedByStrings
main =
    WithTron.Backed.byStrings
        (Option.toHtml Dock.middleRight Theme.dark)
        sendUpdate
        ExampleGui.gui


