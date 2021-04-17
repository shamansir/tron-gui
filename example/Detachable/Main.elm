port module Detachable.Main exposing (main)


import Url exposing (Url)

import Browser exposing (element)
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, div)
import Html.Attributes as Attr exposing (class)

import WithTron exposing (ProgramWithTron)
import Tron exposing (Tron, init, view, update, subscriptions)
import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock
import Tron.Detach as Detach exposing (fromUrl)
import Tron.Expose.Data as Exp exposing (RawProperty, RawInUpdate, RawOutUpdate)
import Tron.Builder as Builder exposing (map)
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


port receieveUpdateFromWs : (Exp.RawInUpdate -> msg) -> Sub msg

port sendUpdateToWs : Exp.RawOutUpdate -> Cmd msg

port ackToWs : Exp.Ack -> Cmd msg
