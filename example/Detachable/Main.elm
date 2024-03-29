port module Detachable.Main exposing (main)


import WithTron as WithTron
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock
import Tron.Tree.Expose.Data as Exp
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication


import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


main : WithTron.Program () Example.Model Example.Msg
main =
    WithTron.application
        (Render.toHtml Dock.center Theme.light)
        (Communication.detachable
            { ack = ackToWs
            , transmit = sendUpdateToWs
            , receive = receieveUpdateFromWs identity
            }
        )
        { for = \_ -> ExampleGui.for
        , init = \_ _ _ -> Example.init
        , view =
            \tree model ->
                { title = "Detachable Tron"
                , body = [ Example.view tree model ]
                }
        , update = Example.update
        , subscriptions = always Example.subscriptions
        , onUrlChange = \_ -> Example.NoOp
        , onUrlRequest = \_ -> Example.NoOp
        }


port receieveUpdateFromWs : (Exp.In -> msg) -> Sub msg

port sendUpdateToWs : Exp.Out -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
