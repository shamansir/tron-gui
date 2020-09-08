module RandomGui exposing (generator)


import Random

import Gui.Gui exposing (Gui)
import Gui.Control exposing (Control(..))
import Gui.Property  exposing (Property(..), Axis)
import Gui.Property as Gui exposing (Label, ToggleState(..))


generator : Random.Generator (Property ())
generator =
    Random.int 0 8
        |> Random.andThen
            (\n ->
                case n of
                    0 -> Random.constant Nil
                    1 -> number |> Random.map Number
                    2 -> coordinate |> Random.map Coordinate
                    3 -> text |> Random.map Text
                    4 -> text |> Random.map Text -- TODO: color
                    5 -> toggle |> Random.map Toggle
                    6 -> button |> Random.map Action
                    7 -> Random.constant Nil -- TODO
                    8 -> Random.constant Nil -- TODO
                    _ -> Random.constant Nil
            )


handler : Random.Generator (a -> ())
handler =
    Random.constant <| always ()


axis : Random.Generator Axis
axis =
    Random.float -100 100
        |> Random.andThen
            (\min ->
                Random.map3
                    Axis
                    (Random.constant min)
                    (Random.float min 100)
                    (Random.float 0 5)
            )


number : Random.Generator (Control Axis Float ())
number =
    axis
        |> Random.andThen
            (\theAxis ->
                Random.map3
                    Control
                    (Random.constant theAxis)
                    (Random.float theAxis.min theAxis.max)
                    handler
            )


coordinate : Random.Generator (Control ( Axis, Axis ) ( Float, Float ) ())
coordinate =
    Random.map2 Tuple.pair axis axis
        |> Random.andThen
            (\( xAxis, yAxis ) ->
                Random.map3
                    Control
                    (Random.constant ( xAxis, yAxis ))
                    (Random.map2
                        Tuple.pair
                        (Random.float xAxis.min xAxis.max)
                        (Random.float yAxis.min yAxis.max)
                    )
                    handler
            )


text : Random.Generator (Control () String ())
text =
    Random.constant
        <| Control () "" <| always ()


button : Random.Generator (Control ( Maybe a ) () ())
button =
    Random.constant
        <| Control Nothing () <| always ()


toggle : Random.Generator (Control () ToggleState ())
toggle =
    Random.int 0 100
        |> Random.map (\n -> if n < 40 then TurnedOff else TurnedOn)
        |> Random.map
            (\tState ->
                Control
                    ()
                    tState
                    <| always ()
            )
