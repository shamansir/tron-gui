port module ForTiler.Main exposing (..)


import Tron
import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Tree.Expose as Exp

import WithTron


import Example.Tiler.Gui as ExampleGui
import Example.Tiler.Logic as Example


main : WithTron.Program () Example.Model Example.Msg
main =
    WithTron.element
        (Render.toHtml Dock.bottomCenter Theme.dark)
        (Communication.sendReceiveJson
            { ack = ack
            , transmit = transmit
            , apply = apply identity
            }
        )
        { for =
            \tree model ->
                ExampleGui.gui tree model
                    |> Tron.lift
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
            \tree model ->
                ExampleGui.for model
                    |> Tron.map ( always Example.NoOp )
        , init = always <| always Example.init
        , update = \msg _ model -> Example.update msg model
        , view = always Example.view
        , subscriptions = always Example.subscriptions
        } -}


port ack : Exp.Tree -> Cmd msg

port transmit : Exp.Out -> Cmd msg

port apply : (List Exp.DeduceIn -> msg) -> Sub msg