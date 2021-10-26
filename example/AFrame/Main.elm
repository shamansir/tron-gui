module AFrame.Main exposing (main)


import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Theme as Theme
import WithTron


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


main : WithTron.Program () Example.Model Example.Msg
main =
    WithTron.sandbox
        (Render.toVr Theme.light)
        { for = ExampleGui.for
        , init = Example.init_
        , view = Example.view_
        , update = Example.update_
        }
