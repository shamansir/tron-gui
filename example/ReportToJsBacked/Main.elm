port module ReportToJsBacked.Main exposing (..)


import Gui.Style.Theme as Theme
import Gui.Style.Dock as Dock
import Gui.Option as Option
import Gui.WithGui as WithGui exposing (BackedWithGui)


import Example.Unit.Gui as ExampleGui


port sendUpdate : ( String, String ) -> Cmd msg


main : BackedWithGui
main =
    WithGui.backed
        (Option.toHtml Dock.middleRight Theme.dark)
        (Option.sendStrings
            { transmit = sendUpdate
            }
        )
        ExampleGui.gui


