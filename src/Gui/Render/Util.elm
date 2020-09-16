module Gui.Render.Util exposing (..)


import Svg exposing (Svg)
import Svg.Attributes as SA


positionAt : Float -> Float -> Svg msg -> Svg msg
positionAt x y s =
    Svg.g
        [ SA.style <| "transform: translate("
            ++ String.fromFloat x ++ "px,"
            ++ String.fromFloat y ++ "px)"
        ]
        [ s ]


positionAt_ : { a | x : Float, y : Float } -> Svg msg -> Svg msg
positionAt_ pos =
    positionAt pos.x pos.y


textAt : Float -> Float -> String -> Svg msg
textAt x y string =
    positionAt x y
        <| Svg.text_ []
        <| List.singleton
        <| Svg.text string


rect_ : String -> { a | width : Float, height : Float } -> Svg msg
rect_ fill b =
    Svg.rect
        [ SA.fill fill
        -- , S.x <| String.fromFloat b.x ++ "px"
        -- , S.y <| String.fromFloat b.y ++ "px"
        , SA.width <| String.fromFloat b.width ++ "px"
        , SA.height <| String.fromFloat b.height ++ "px"
        , SA.stroke "black"
        , SA.strokeWidth "1"
        ]
        []
