port module ReportToJsJson.Main exposing (main)


import Html

import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Expose.Data as Exp
import WithTron exposing (ProgramWithTron)
import Tron.Option as Option exposing (..)

import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


{-
-- Change to `Default` example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui
-}


{-
-- Change to `Unit` example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Unit.Main as Example
import Example.Unit.Model as Example
import Example.Unit.Msg as Example
import Example.Unit.Gui as ExampleGui
-}


main : ProgramWithTron () Example.Model Example.Msg
main =
    WithTron.element
        (Option.toHtml Dock.middleRight Theme.dark)
        (Option.sendJson
            { ack = initGui
            , transmit = sendUpdate
            }
        )
        { for = ExampleGui.for
        , init = always Example.init
        , view = always <| Html.div [] [] -- Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }


port sendUpdate : Exp.RawOutUpdate -> Cmd msg

port initGui : Exp.RawProperty -> Cmd msg
