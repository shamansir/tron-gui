port module ForTiler.Main exposing (..)


import Tron.Style.Theme as Theme
import Tron.Style.Dock as Dock
import Tron.Option as Option
import Tron.Expose.Data as Exp

import WithTron.Backed as WithTron exposing (BackedWithTron)


import Example.Tiler.Gui as ExampleGui


port ack : Exp.RawProperty -> Cmd msg

port transmit : Exp.RawOutUpdate -> Cmd msg


main : BackedWithTron
main =
    WithTron.backed
        (Option.toHtml Dock.bottomCenter Theme.dark)
        ( ack, transmit )
        ExampleGui.gui


