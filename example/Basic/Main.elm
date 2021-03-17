module Basic.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Gui as Tron exposing (Gui, Msg, init, view, update, subscriptions)
import Gui.Build as Builder exposing (map)
import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock as Dock
import Gui.Option as Option
import Gui.WithGui as WithGui exposing (ProgramWithGui)


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


main : ProgramWithGui () Example.Model Example.Msg
main =
    WithGui.element
        (Option.toHtml Dock.center Theme.dark)
        Option.noCommunication
        { for = ExampleGui.for
        , init = always Example.init
        , view = Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }
