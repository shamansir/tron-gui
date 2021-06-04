port module ForTiler.Main exposing (..)


import Tron
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option
import Tron.Expose.Data as Exp

import WithTron.Backed exposing (AppBackedByProxy)


import Example.Tiler.Gui as ExampleGui
import Example.Tiler.Logic as Example


main : AppBackedByProxy () Example.Model Example.Msg
main =
    WithTron.Backed.byProxyApp
        (Option.toHtml Dock.bottomCenter Theme.dark)
        ( ack, transmit, receive identity )
        { for =
            \valueAt model ->
                ExampleGui.gui valueAt model
                    |> Tron.map ( always Example.NoOp )
        , init = Example.init
        , update = Example.update
        , view = Example.view
        , subscriptions = Example.subscriptions
        }


port ack : Exp.RawProperty -> Cmd msg

port transmit : Exp.RawOutUpdate -> Cmd msg

port receive : (Exp.RawInUpdate -> msg) -> Sub msg