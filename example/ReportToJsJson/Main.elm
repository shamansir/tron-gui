port module ReportToJsJson.Main exposing (main)


import Html

import Gui.Style.Theme as Theme
import Gui.Style.Dock as Dock
import Gui.Expose as Exp
import Gui.WithGui as WithGui
import Gui.WithGui exposing (ProgramWithGui)
import Gui.Option as Option exposing (..)

import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


{-
-- Change to a more boring example
-- by just commenting out `.Goose` imports above
-- and removing the comment here
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example
import Example.Default.Gui as ExampleGui
-}


main : ProgramWithGui () Example.Model Example.Msg
main =
    WithGui.element
        [ Option.sendJson
                { ack = initGui
                , transmit = sendUpdate
                }
        , Option.appearance Dock.middleRight Theme.dark
        ]
        { for = ExampleGui.for
        , init = always ( Example.init, Cmd.none )
        , view = always <| Html.div [] [] -- Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }


port sendUpdate : Exp.RawUpdate -> Cmd msg

port initGui : Exp.RawProperty -> Cmd msg
