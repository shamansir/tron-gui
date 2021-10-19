module Basic.Main exposing (main)


import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock
import Tron.Option.Render as Option
import Tron.Option.Communication as Option
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
        { for = always ExampleGui.for
        , init = always Example.init
        , view = always Example.view
        , update = \msg _ model -> Example.update msg model
        , subscriptions = always <| always Sub.none
        }