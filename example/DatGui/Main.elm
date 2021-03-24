port module DatGui.Main exposing (main)


import Browser exposing (element)
import Html exposing (Html, div)

import Gui as Tron exposing (Gui, view)
import Gui.Expose as Exp exposing (RawProperty, RawUpdate)
import Gui.WithGui as WithGui exposing (ProgramWithGui)
import Gui.Option as Option

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


main : ProgramWithGui () Example.Model Example.Msg
main =
    WithGui.element
        Option.hidden
        (Option.withDatGui
            { ack = startDatGui
            , receive = updateFromDatGui
            })
        { for = ExampleGui.for
        , init = always Example.init
        , view = Example.view
        , update = Example.update
        , subscriptions = Example.subscriptions
        }


port updateFromDatGui : (Exp.RawUpdate -> msg) -> Sub msg

port startDatGui : Exp.RawProperty -> Cmd msg
