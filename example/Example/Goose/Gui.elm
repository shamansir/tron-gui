module Example.Goose.Gui exposing (for)


import Tron exposing (Tron)
import Tron.Builder as Gui
import Tron.Style.PanelShape exposing (cols)
import Tron.Style.PanelShape as Shape exposing (auto)
import Tron.Style.CellShape as Cell exposing (single)

import Example.Goose.Model exposing (..)
import Example.Goose.Msg exposing (..)


for : Model -> Tron Msg
for model =
    Gui.root
        [
            ( "honk on"
            ,
                Gui.toggle
                    (Tuple.first model.honk)
                    (\v -> if v then HonkOn else HonkOff)
            )
        ,
            ( "honk"
            ,
                if Tuple.first model.honk then
                    Tuple.second model.honk
                        |> honkGui
                        |> Gui.expand
                else Gui.none
            )
        ,
            ( "eye"
            , eyeGui model.eye
            )
        ,
            ( "look at"
            ,
                Gui.choiceByCompare
                    ( Shape.auto )
                    Cell.single -- Cell.halfByOne
                    (\v ->
                        case v of
                            Left -> "left"
                            Right -> "right"
                    )
                    [ Left, Right ]
                    model.lookAt
                    compareDirections
                    LookAt
            )
        ,
            ( "punk on"
            ,
                Gui.toggle
                    model.punk
                    (\v -> if v then PunkOn else PunkOff)
            )
        ,
            ( "colors"
            , colorsGui model.punk model.colors
            )
        ,
            ( "boots on"
            ,
                Gui.toggle
                    (
                        case model.shoes of
                            None -> False
                            Boots -> True
                    )
                    (\v -> ChangeShoes <| if v then Boots else None)
            )
        ]


honkGui : HonkConfig -> Tron Msg
honkGui config =
    Gui.nest
        ( cols 2 )
        Cell.single
        [
            ( "position"
            ,
                let posAxis = { min = -50, max = 50, step = 1 }
                in Gui.xy
                    ( posAxis, posAxis )
                    config.position
                    ChangeHonkTextPosition
            )
        ,
            ( "size"
            ,
                Gui.text
                    (String.fromInt <| config.size)
                    (String.toInt
                        >> Maybe.withDefault (default.honk |> Tuple.second |> .size)
                        >> ChangeHonkTextSize)
            )
        ,
            ( "text"
            ,
                Gui.text
                    config.text
                    ChangeHonkText
            )
        ,
            ( "color"
            ,
                Gui.color
                    config.color
                    ChangeHonkTextColor
            )
        ]


eyeGui : EyeConfig -> Tron Msg
eyeGui config =
    Gui.nest
        ( cols 1 )
        Cell.single
        [
            ( "position"
            ,
                let posAxis = { min = -5, max = 5, step = 1 }
                in Gui.xy
                    ( posAxis, posAxis )
                    config.position
                    ChangeEyePosition
            )
        ,
            ( "size"
            ,
                Gui.number
                    { min = 1, max = 10, step = 0.1 }
                    config.size
                    ChangeEyeSize
            )
        ]


colorsGui : Bool -> Colors -> Tron Msg
colorsGui isPunk colors =
    Gui.nest
        ( cols 2 )
        Cell.single
        [ ( "eye", Gui.color colors.eye ChangeEyeColor )
        , ( "feathers", Gui.color colors.feathers ChangeFeathersColor )
        , ( "skin", Gui.color colors.skin ChangeSkinColor )
        , ( "background", Gui.color colors.background ChangeBackground )
        ,
            ( "iroquois"
            , if isPunk
                then Gui.color colors.iroquois ChangeIroquoisColor
                else Gui.none
            )
        ]


compareDirections : LookDirection -> LookDirection -> Bool
compareDirections dirA dirB =
    case ( dirA, dirB ) of
        ( Left, Left ) -> True
        ( Right, Right ) -> True
        _ -> False
