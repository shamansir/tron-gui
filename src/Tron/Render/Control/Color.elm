module Tron.Render.Control.Color exposing (..)


import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Color as Color

import Tron.Render.Context as Context exposing (Context)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Coloring as Coloring exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA


view : Theme -> Context -> Color.Control a -> Svg msg
view theme ctx (Control _ ( _, value ) _) =
    viewValue theme ctx value


type alias ColorParam =
    { label : String
    , stops : List String
    }


colorParams : List ColorParam
colorParams =
    [ { label = "Hue", stops = [ "hsla(0deg,   100%,  50%, 1)", "hsla(359deg, 100%,  50%, 1)" ] }
    , { label = "Sat", stops = [ "hsla(360deg, 0%,    50%, 1)", "hsla(360deg, 100%,  50%, 1)" ] }
    , { label = "Val", stops = [ "hsla(360deg, 100%,   0%, 1)", "hsla(360deg, 100%, 100%, 1)" ] }
    , { label = "Alf", stops = [ "hsla(360deg, 100%, 100%, 0)", "hsla(360deg, 100%, 100%, 1)" ] }
    ]


viewSliders : Theme -> Context -> Color -> Svg msg
viewSliders theme ctx value =
    let
        bounds = ctx.bounds
        gap = 10.0
        hsla = Color.toHsla value
        ( paramWidth, paramHeight ) = ( bounds.width - gap * 2, bounds.height / 4 )
        paramBox index param =
            Svg.g
                []
                [ Svg.defs
                    []
                    [ Svg.linearGradient
                        [ SA.id <| "fill" ++ String.fromInt index
                        ]
                        <| List.indexedMap
                            (\stopId stop ->
                                Svg.stop
                                    [ SA.offset <| (String.fromFloat <| Basics.toFloat stopId / (Basics.toFloat <| List.length param.stops - 1) * 100) ++ "%"
                                    , SA.stopColor stop
                                    ]
                                    []
                            )
                        <| param.stops
                    ]
                , Svg.rect
                    [ SA.x <| String.fromFloat <| gap
                    , SA.y <| String.fromFloat <| Basics.toFloat index * paramHeight
                    , SA.width <| String.fromFloat paramWidth
                    , SA.height <| String.fromFloat paramHeight
                    , SA.fill <| "url(#fill" ++ String.fromInt index ++ ")"
                    ]
                    []
                , Svg.text_
                    [ SA.fontSize <| (String.fromFloat 9.0 ++ "px")
                    , SA.x <| String.fromFloat <| gap
                    , SA.y <| String.fromFloat <| Basics.toFloat index * paramHeight
                    ]
                    [ Svg.text param.label
                    ]
                ]
                -- [ SA.transform |> positionAt ]
    in Svg.g
        []
        <| List.indexedMap paramBox
        <| colorParams



viewValue : Theme -> Context -> Color -> Svg msg
viewValue theme ctx value =
    let
        bounds = ctx.bounds
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        radius = (min bounds.width bounds.height) / 6
    in Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r <| String.fromFloat radius
        , SA.fill <| Color.toCssString value
        , SA.strokeWidth "1"
        , SA.stroke <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
        ]
        [
        ]
