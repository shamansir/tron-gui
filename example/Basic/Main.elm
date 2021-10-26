module Basic.Main exposing (main)


import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock
import Tron.Option.Render as Render
import WithTron as WithTron


import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


main : WithTron.Program () Example.Model Example.Msg
main =
    WithTron.sandbox
        (Render.toHtml Dock.center Theme.dark)
        { for = ExampleGui.for
        , init = Example.init_
        , view = Example.view_
        , update = Example.update_
        }