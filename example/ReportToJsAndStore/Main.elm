module ReportToJsAndStore.Main exposing (..)


import Html

import Gui.Style.Theme as Theme
import Gui.Style.Dock as Dock
import Gui.Option as Option
import Gui.WithGui as WithGui exposing (ProgramWithGui)


import Example.Unit.Main as Example
import Example.Unit.Model as Example
import Example.Unit.Msg as Example
import Example.Unit.Gui as ExampleGui


type alias Model = Dict String String


type alias Msg = ( String, String )


init = ( Dict.empty, Cmd.none )


update ( key, value ) dict = ( dict |> Dict.insert key value , Cmd.none )


view _ = Htm.div [] []


subscriptions _ = Sub.none


port sendUpdate : ( String, String ) -> Cmd msg


main : ProgramWithGui () Model Msg
main =
    WithGui.element
        [ Option.sendStrings
            { transmit = sendUpdate
            }
        , Option.appearance Dock.middleRight Theme.dark
        ]
        { for = ExampleGui.for
        , init = always Example.init
        , view = always <| Html.div [] [] -- Example.view
        , update = Example.update
        , subscriptions = always Sub.none
        }


