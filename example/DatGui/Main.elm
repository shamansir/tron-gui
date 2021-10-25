port module DatGui.Main exposing (main)


import Tron.Tree.Expose as Exp
import WithTron
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication

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
    WithTron.element
        Render.hidden
        (Communication.withDatGui
            { ack = startDatGui
            , receive = updateFromDatGui identity
            })
        { for = always ExampleGui.for
        , init = always ( Example.init, Cmd.none )
        , view = always Example.view
        , update = \msg _ model -> ( Example.update msg model, Cmd.none )
        , subscriptions = \_ _ -> Sub.none
        }


port updateFromDatGui : (Exp.In -> msg) -> Sub msg

port startDatGui : Exp.Tree -> Cmd msg
