port module Detachable.Main exposing (main)


import WithTron exposing (ProgramWithTron)
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock
import Tron.Expose.Data as Exp
import Tron.Option as Option


import Example.Goose.Main as Example
import Example.Goose.Model as Example
import Example.Goose.Msg as Example
import Example.Goose.Gui as ExampleGui


main : ProgramWithTron () Example.Model Example.Msg
main =
    WithTron.application
        (Option.toHtml Dock.center Theme.light)
        (Option.detachable
            { ack = ackToWs
            , transmit = sendUpdateToWs
            , receive = receieveUpdateFromWs identity
            }
        )
        { for = ExampleGui.for
        , init = always Example.init
        , view =
            \model ->
                { title = "Detachable Tron"
                , body = [ Example.view model ]
                }
        , update = Example.update
        , subscriptions = always Sub.none
        , onUrlChange = always Example.NoOp
        , onUrlRequest = always Example.NoOp
        }


port receieveUpdateFromWs : (Exp.In -> msg) -> Sub msg

port sendUpdateToWs : Exp.Out -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
