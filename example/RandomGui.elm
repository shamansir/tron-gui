module RandomGui exposing (generator)


import Random
import Array exposing (Array)

import Gui.Gui exposing (Gui)
import Gui.Control exposing (Control(..))
import Gui.Property  exposing (Property(..), Axis, ChoiceControl, GroupControl)
import Gui.Property as Gui exposing (Label, ToggleState(..), ExpandState(..))


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
                    7 -> choice |> Random.map Choice
                    8 -> group |> Random.map Group
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


controls : Random.Generator (Array ( Label, Property () ) )
controls =
    let
        addLabel : Property () -> Random.Generator ( Label, Property () )
        addLabel prop =
            -- TODO: case prop of
            Random.constant ( "foo", prop )
    in
        Random.int 2 10
            |> Random.andThen
                (\count ->
                    Random.list count
                        (Random.lazy <| always <| Random.andThen addLabel <| generator)
                        |> Random.map Array.fromList
                )


choice : Random.Generator (ChoiceControl ())
choice =
    controls
        |> Random.andThen
            (\cs ->
                Random.map3
                    Control
                    (Random.map2
                        Tuple.pair
                        (shape <| Array.length cs)
                        (Random.constant cs)
                    )
                    (Random.map2
                        Tuple.pair
                        expandState
                        (Random.int 0 <| Array.length cs - 1)
                    )
                    handler
            )


group : Random.Generator (GroupControl ())
group =
    controls
        |> Random.andThen
            (\cs ->
                Random.map3
                    Control
                    (Random.map2
                        Tuple.pair
                        (shape <| Array.length cs)
                        (Random.constant cs)
                    )
                    (Random.map2
                        Tuple.pair
                        expandState
                        (Random.constant Nothing)
                    )
                    handler
            )


shape : Int -> Random.Generator Gui.Shape
shape toFit =
    Random.int 1 toFit
        |> Random.map (\v -> ( v, (toFit // v) + 1 ))


toggleState : Random.Generator ToggleState
toggleState =
    Random.int 0 100
        |> Random.map (\n -> if n < 40 then TurnedOff else TurnedOn)


expandState : Random.Generator ExpandState
expandState =
    Random.int 0 100
        |> Random.map (\n -> if n < 40 then Collapsed else Expanded)


toggle : Random.Generator (Control () ToggleState ())
toggle =
    toggleState
        |> Random.map
            (\tState ->
                Control
                    ()
                    tState
                    <| always ()
            )
