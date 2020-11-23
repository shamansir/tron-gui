module Example.Goose.Gui exposing (..)


import Gui exposing (Gui)
import Gui.Build as Gui
import Gui.Style.Shape exposing (cols)
import Gui.Style.Shape as Shape exposing (auto)
import Gui.Style.CellShape as Cell exposing (single)

import Example.Goose.Model exposing (..)
import Example.Goose.Msg exposing (..)


for : Model -> Gui.Builder Msg
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
                    honkGui <| Tuple.second model.honk
                else Gui.none
            )
        ,
            ( "eye"
            , eyeGui model.eye
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
        ]


honkGui : HonkConfig -> Gui.Builder Msg
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
                    ChangeEyePosition
            )
        ,
            ( "size"
            ,
                Gui.text
                    (String.fromInt <| config.size)
                    (String.toInt
                        >> Maybe.withDefault (init.honk |> Tuple.second |> .size)
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


eyeGui : EyeConfig -> Gui.Builder Msg
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
        ,
            ( "look at"
            ,
                Gui.choice
                    ( Shape.auto )
                    Cell.single -- Cell.halfByOne
                    (\v ->
                        case v of
                            Left -> "left"
                            Right -> "right")
                    [ Left, Right ]
                    config.lookAt
                    (==)
                    LookAt
            )
        ]


colorsGui : Bool -> Colors -> Gui.Builder Msg
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
