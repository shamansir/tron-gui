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


polarToCartesian
    :  { x : Float, y : Float }
    -> { radiusA : Float, radiusB : Float }
    -> Float
    -> { x : Float, y : Float }
polarToCartesian center { radiusA, radiusB } angleInDegrees =
    let
        angleInRadians = (angleInDegrees-90) * pi / 180.0
    in
        { x = center.x + (radiusA * cos angleInRadians)
        , y = center.y + (radiusB * sin angleInRadians)
        }


describeArc
    :  { x : Float, y : Float }
    -> { radiusA : Float, radiusB : Float }
    -> { from : Float, to : Float }
    -> String
describeArc center ({ radiusA, radiusB } as radii) angles =
    let
        start = polarToCartesian center radii angles.to
        end = polarToCartesian center radii angles.from
        arcSweep = if (angles.to - angles.from) <= 180 then 0 else 1
    in
        [ "M", String.fromFloat start.x, String.fromFloat start.y
        , "A", String.fromFloat radiusA, String.fromFloat radiusB
                , String.fromInt 0, String.fromInt arcSweep, String.fromInt 0
                , String.fromFloat end.x, String.fromFloat end.y
        ] |> String.join " "


describeMark
    :  { x : Float, y : Float }
    -> { radiusA : Float, radiusB : Float }
    -> Float
    -> String
describeMark center { radiusA, radiusB } angleInDegrees =
    let
        angleInRadians = (angleInDegrees-90) * pi / 180.0
        m_radiusA = radiusA * 0.45
        m_radiusB = radiusB * 0.45
        start =
            { x = center.x + m_radiusA * cos angleInRadians
            , y = center.y + m_radiusB * sin angleInRadians
            }
        end =
            { x = center.x + radiusA * cos angleInRadians
            , y = center.y + radiusB * sin angleInRadians
            }
    in
        [ "M", String.fromFloat start.x, String.fromFloat start.y
        , "L", String.fromFloat end.x, String.fromFloat end.y
        ] |> String.join " "


