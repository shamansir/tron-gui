module RandomGui exposing (generator)


import Random
import Array exposing (Array)
import Color exposing (..)
import Axis exposing (Axis)

import Gui exposing (Gui)
import Gui.Control as Core exposing (Control(..))
import Gui.Control.Nest exposing (ChoiceControl, GroupControl, NestState(..), SelectedAt(..), expand, getItems)
import Gui.Control.Toggle exposing (ToggleState(..))
import Gui.Control.Text exposing (TextState(..))
import Gui.Control.Button as Button exposing (Face(..), Icon(..))
import Gui.Property  exposing (Property(..))
import Gui.Property as Gui exposing ( Label )
import Gui.Build exposing (Builder)
import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CS exposing (..)


type DeepLevel = DeepLevel Int


type Icon
    = Arrow
    | Export
    | Arm
    | Regenerate
    | Goose


generator : Random.Generator (Builder ())
generator =
    group (DeepLevel 0)
        |> Random.map expand
        |> Random.andThen group_


property : DeepLevel -> Random.Generator (Builder ())
property (DeepLevel deep) =
    Random.int 1 8
        |> Random.andThen
            (\n ->
                case n of
                    0 -> Random.constant Nil
                    1 -> number |> Random.map Number
                    2 -> coordinate |> Random.map Coordinate
                    3 -> text |> Random.map Text
                    4 -> color |> Random.map Color
                    5 -> toggle |> Random.map Toggle
                    6 -> button |> Random.map Action
                    7 ->
                        if deep < 7 then
                            choice (DeepLevel <| deep + 1)
                                |> Random.andThen choice_
                        else button |> Random.map Action
                    8 ->
                        if deep < 7 then
                            group (DeepLevel <| deep + 1)
                                |> Random.andThen group_
                        else button |> Random.map Action
                    _ -> Random.constant Nil
            )


handler : Random.Generator (Maybe ( a -> () ))
handler =
    Random.constant <| Just <| always ()


roundBy : Int -> Float -> Float
roundBy _ f =
    toFloat (floor (f * 100)) / 100


axis : Random.Generator Axis
axis =
    Random.float -100 100
        |> Random.andThen
            (\min ->
                Random.map3
                    Axis
                    (Random.constant min
                        |> Random.map (roundBy 2))
                    (Random.float min 100
                        |> Random.map (roundBy 2))
                    (Random.float 0 5
                        |> Random.map (roundBy 2))
            )


number : Random.Generator ( Control Axis Float () )
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


coordinate : Random.Generator ( Control ( Axis, Axis ) ( Float, Float ) () )
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


text : Random.Generator ( Control () ( TextState, String ) () )
text =
    Random.constant
        <| Control () ( Ready, "foobar" ) <| Just <| always ()


color : Random.Generator (Control () Color ())
color =
    Random.map3
        Control
        (Random.constant ())
        randomColor
        handler


button : Random.Generator ( Control Face () () )
button =
    Random.uniform Nothing [ Just Arrow, Just Export, Just Regenerate, Just Arm, Just Goose ]
        |> Random.map (Maybe.map (sourceOf >> Button.Icon >> WithIcon) >> Maybe.withDefault Default)
        |> Random.map (\icon -> Control icon () <| Just <| always ())


controls : DeepLevel -> Random.Generator ( Array ( Label, Builder () ) )
controls deep =
    let
        labelFor prop =
            case prop of
                Nil -> "ghost"
                Number _ -> "num"
                Coordinate _ -> "coord"
                Text _ -> "text"
                Color _ -> "color"
                Toggle _ -> "toggle"
                Action _ -> "button"
                Choice _ _ _ -> "choice"
                Group _ _ _ -> "group"
        addLabel : Builder () -> Random.Generator ( Label, Builder () )
        addLabel prop =
            Random.int 0 10000
                |> Random.map String.fromInt
                |> Random.map (\n -> labelFor prop ++ "-" ++ n)
                |> Random.map (\label -> ( label, prop ))
    in
        Random.int 2 10
            |> Random.andThen
                (\count ->
                    Random.list count
                        (Random.andThen addLabel <| Random.lazy (\_ -> property deep))
                        |> Random.map Array.fromList
                )


choice : DeepLevel -> Random.Generator ( ChoiceControl (Label, Builder ()) () )
choice deep =
    controls deep
        |> Random.andThen
            (\cs ->
                Random.map3
                    Control
                    {- (Random.map2
                            Tuple.pair
                            (shape <| Array.length cs)
                            (Random.constant CS.default)
                        ) -}
                    (Random.constant cs)
                    (Random.map2
                        Tuple.pair
                        expandState
                        (Random.map SelectedAt
                            <| Random.int 0 <| Array.length cs - 1)
                    )
                    handler
            )


group : DeepLevel -> Random.Generator ( GroupControl ( Label, Builder () ) () )
group deep =
    controls deep
        |> Random.andThen
            (\cs ->
                Random.map3
                    Control
                    (Random.constant cs)
                    (Random.map2
                        Tuple.pair
                        expandState
                        (Random.constant ())
                    )
                    handler
            )


shapeFor
     : Control (Array ( Label, Builder () )) val msg
    -> Random.Generator ( Gui.Shape, CS.CellShape )
shapeFor cs =
    Random.map2
        Tuple.pair
        (shape <| Array.length <| getItems cs)
        (Random.constant CS.default)


shape : Int -> Random.Generator Gui.Shape
shape toFit =
    Random.int 1 toFit
        |> Random.map
            (\v ->
                ( v
                , (toFit // v) + (if toFit - (toFit // v * toFit) > 0 then 1 else 0)
                )
            )
        |> Random.map (Tuple.mapBoth toFloat toFloat)


group_
     : GroupControl (Label, Builder ()) ()
    -> Random.Generator (Builder ())
group_ control =
    Random.map
        (\s -> Group Nothing s control)
        <| shapeFor control


choice_
     : ChoiceControl (Label, Builder ()) ()
    -> Random.Generator (Builder ())
choice_ control =
    Random.map
        (\s -> Choice Nothing s control)
        <| shapeFor control


toggleState : Random.Generator ToggleState
toggleState =
    Random.int 0 100
        |> Random.map (\n -> if n < 40 then TurnedOff else TurnedOn)


expandState : Random.Generator NestState
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
                    <| Just
                    <| always ()
            )


sourceOf : Icon -> String
sourceOf icon =
    case icon of
        Arm -> "arm"
        Arrow -> "arrow"
        Export -> "export"
        Regenerate -> "regenerate"
        Goose -> "cai"


randomColor : Random.Generator Color
randomColor =
    Random.map3
        Color.rgb
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)
