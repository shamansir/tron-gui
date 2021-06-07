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
        ( ack, transmit, apply identity )
        { for =
            \valueAt model ->
                ExampleGui.gui valueAt model
                    |> Tron.map ( always Example.NoOp )
        , init = Example.init
        , update = Example.update
        , view = Example.view
        , subscriptions = Example.subscriptions
        }


{- import Example.Default.Gui as ExampleGui
import Example.Default.Main as Example
import Example.Default.Model as Example
import Example.Default.Msg as Example


main : AppBackedByProxy () Example.Model Example.Msg
main =
    WithTron.Backed.byProxyApp
        (Option.toHtml Dock.bottomCenter Theme.dark)
        ( ack, transmit, apply identity )
        { for =
            \valueAt model ->
                ExampleGui.for model
                    |> Tron.map ( always Example.NoOp )
        , init = always <| always Example.init
        , update = \msg _ model -> Example.update msg model
        , view = always Example.view
        , subscriptions = always Example.subscriptions
        } -}


port ack : Exp.Property -> Cmd msg

port transmit : Exp.Out -> Cmd msg

port apply : (Exp.DeduceIn -> msg) -> Sub msg