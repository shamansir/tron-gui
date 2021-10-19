port module DatGui.Main exposing (main)


import Tron.Expose.Data as Exp
import WithTron exposing (ProgramWithTron)
import Tron.Option as Option

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
        Option.hidden
        (Option.withDatGui
            { ack = startDatGui
            , receive = updateFromDatGui identity
            })
        { for = ExampleGui.for
        , init = always Example.init
        , view = Example.view
        , update = Example.update
        , subscriptions = Example.subscriptions
        }


port updateFromDatGui : (Exp.In -> msg) -> Sub msg

port startDatGui : Exp.Tree -> Cmd msg
