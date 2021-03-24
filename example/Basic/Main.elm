module Basic.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import Tron exposing (Tron, Msg, init, view, update, subscriptions)
import Tron.Builder as Builder exposing (map)
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock
import Tron.Option as Option
import WithTron exposing (ProgramWithTron)


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


main : ProgramWithTron () Example.Model Example.Msg
main =
    WithTron.element
        (Option.toHtml Dock.center Theme.dark)
        Option.noCommunication
        { for = ExampleGui.for
        , init = always Example.init
        , view = Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }
