module Tron.Tree.Random exposing (generator)


{-| Using this module, you may generate the random UI structure if you would ever want to, for testing purposes, for example.

# Generate random UI

@docs generator

See also: `Tron.OfValue`, `WithTron.for`, `Tron.OfValue.lift`
-}

import Random
import Array exposing (Array)
import Color exposing (Color)
import Axis exposing (Axis)
import Url.Builder as Url

import Tron exposing (Tron)
import Tron.Control as Core exposing (Control(..))
import Tron.Control.Impl.Nest as Nest exposing (ChoiceControl, GroupControl, Form(..), ItemId, expand, getItems)
import Tron.Control.Impl.Toggle as Toggle exposing (ToggleState(..))
import Tron.Control.Impl.Text as Text exposing (TextState(..))
import Tron.Control.Impl.Button as Button exposing (Face(..), Icon(..))
import Tron.Control.Impl.Number as Number
import Tron.Control.Impl.XY as XY
import Tron.Control.Impl.Color as Color
import Tron.Path as Path

import Tron.Tree.Internals  exposing (Tree(..))
import Tron.Style.PanelShape exposing (PanelShape, cols)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS exposing (..)
import Tron.Style.Theme exposing (Theme(..))


type DeepLevel = DeepLevel Int


type Icon
    = Arrow
    | Export
    | Arm
    | Regenerate
    | Goose


{-| -}
generator : Random.Generator (Tree ())
generator =
    group (DeepLevel 0)
        |> Random.map expand
        |> Random.andThen group_


property : DeepLevel -> Random.Generator (Tree ())
property (DeepLevel deep) =
    Random.int 1 9
        |> Random.andThen
            (\n ->
                case n of
                    0 -> Random.constant <| Nil ()
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
                    _ -> Random.constant <| Nil ()
            )


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


number : Random.Generator ( Number.Control () )
number =
    axis
        |> Random.andThen
            (\theAxis ->
                Random.map3
                    Control
                    (Random.constant theAxis)
                    (Random.map2
                        Tuple.pair
                        (Random.constant Nothing)
                        (Random.float theAxis.min theAxis.max)
                    )
                    (Random.constant ())
            )


coordinate : Random.Generator ( XY.Control () )
coordinate =
    Random.map2 Tuple.pair axis axis
        |> Random.andThen
            (\( xAxis, yAxis ) ->
                Random.map3
                    Control
                    (Random.constant ( xAxis, yAxis ))
                    (Random.map2
                        Tuple.pair
                        (Random.constant Nothing)
                        <| Random.map2
                            Tuple.pair
                            (Random.float xAxis.min xAxis.max)
                            (Random.float yAxis.min yAxis.max)
                    )
                    (Random.constant ())
            )


text : Random.Generator ( Text.Control () )
text =
    Random.constant
        <| Control () ( Ready, "foobar" ) ()


color : Random.Generator ( Color.Control () )
color =
    Random.map3
        Control
        (Random.constant ())
        (Random.map2
            Tuple.pair
            (Random.constant Nothing)
            randomColor
        )
        (Random.constant ())


button : Random.Generator ( Control Face () () )
button =
    Random.uniform Nothing [ Just Arrow, Just Export, Just Regenerate, Just Arm, Just Goose ]
        |> Random.map
                (Maybe.map
                    (sourceOf
                        >> Button.themedIcon
                        >> Button.WithIcon
                    )
                >> Maybe.withDefault Default
                )
        |> Random.map (\icon -> Control icon () ())


controls : DeepLevel -> Random.Generator ( Array ( Path.Label, Tree () ) )
controls deep =
    let
        labelFor prop =
            case prop of
                Nil _ -> "ghost"
                Number _ -> "num"
                Coordinate _ -> "coord"
                Text _ -> "text"
                Color _ -> "color"
                Toggle _ -> "toggle"
                Action _ -> "button"
                Choice _ _ _ -> "choice"
                Group _ _ _ -> "group"
                Live innerProp -> labelFor innerProp
        addLabel : Tree () -> Random.Generator ( Path.Label, Tree () )
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


choice : DeepLevel -> Random.Generator ( ChoiceControl (Path.Label, Tree ()) () )
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
                        (\f s ->
                            { form = f
                            , selected = s
                            , face = Nothing
                            , page = 0
                            , prevSelected = Nothing
                            , mode = Nest.Pages
                            }
                        )
                        form
                        (Random.int 0 <| Array.length cs - 1)
                    )
                    (Random.constant ())
            )


group : DeepLevel -> Random.Generator ( GroupControl ( Path.Label, Tree () ) () )
group deep =
    controls deep
        |> Random.andThen
            (\cs ->
                Random.map3
                    Control
                    (Random.constant cs)
                    (Random.map
                        (\f ->
                            { form = f
                            , page = 0
                            , face = Nothing
                            }
                        )
                        form
                    )
                    (Random.constant ())
            )


{- switch : Random.Generator ( Switch.Control () )
switch =
    Random.int 1 9
        |> Random.andThen
            (\howMany ->
                Random.list howMany (Random.int 1 9)
                    |> Random.map (Tuple.pair howMany)
            )
        |> Random.andThen
            (\(howMany, items) ->
                Random.map3
                    Control
                    (Random.constant <| Array.fromList <| List.map String.fromInt <| items)
                    (Random.map2
                        Tuple.pair
                        (Random.constant Nothing)
                        (Random.int 0 howMany)
                    )
                    (Random.constant ())
            ) -}


shapeFor
     : Control (Array ( Path.Label, Tree () )) val msg
    -> Random.Generator ( PanelShape, CS.CellShape )
shapeFor cs =
    Random.map2
        Tuple.pair
        (shape <| Array.length <| getItems cs)
        (Random.constant CS.default)


shape : Int -> Random.Generator PanelShape
shape toFit =
    Random.int 1 toFit
        |> Random.map cols


group_
     : GroupControl (Path.Label, Tree ()) ()
    -> Random.Generator (Tree ())
group_ control =
    Random.map
        (\s -> Group Nothing s control)
        <| shapeFor control


choice_
     : ChoiceControl (Path.Label, Tree ()) ()
    -> Random.Generator (Tree ())
choice_ control =
    Random.map
        (\s -> Choice Nothing s control)
        <| shapeFor control


toggleState : Random.Generator ToggleState
toggleState =
    Random.int 0 100
        |> Random.map (\n -> if n < 40 then TurnedOff else TurnedOn)


form : Random.Generator Form
form =
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
                    ()
            )


sourceOf : Icon -> Theme -> Button.Url
sourceOf icon theme =
    Button.makeUrl <|
        Url.relative
            [ "assets"
            ,
                ( case icon of
                    Arm ->  "arm"
                    Arrow -> "arrow"
                    Export -> "export"
                    Regenerate -> "regenerate"
                    Goose -> "cai"
                )
            ++ "_" ++
                ( case theme of
                    Dark -> "dark"
                    Light -> "light"
                )
            ++ ".svg"
            ]
            []


randomColor : Random.Generator Color
randomColor =
    Random.map3
        Color.rgb
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)
