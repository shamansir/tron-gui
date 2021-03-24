module Tron.Render.Util exposing (..)


import Color exposing (Color)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Url exposing (Url)

import Tron.Render.Transform exposing (..)

import Tron.Style.Theme exposing (Theme)
import Tron.Style.Selected exposing (Selected)
import Tron.Style.Placement exposing (Placement)
import Tron.Focus exposing (Focused)


type alias State = ( Placement, Focused, Selected )


none : Svg msg
none = Svg.text ""


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
        angleInRadians = (angleInDegrees - 90) * pi / 180.0
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
        angleInRadians = (angleInDegrees - 90) * pi / 180.0
        m_radiusA = radiusA * 0.4
        m_radiusB = radiusB * 0.4
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


arrow : Color -> Transform Scale -> Transform Rotate -> Svg msg
arrow fill (Transform s) (Transform r) =
    let
        ( cx, cy ) = ( 14, 14 )
        pointsA =
            [ (0.3,14.5), (14,0.8), (27.7,14.5), (25.8,16.3), (14,4.6), (2.2,16.3), (0.3,14.5) ]
        pointsB =
            [ (15.4,2.7), (15.4,26.7), (12.6,26.7), (12.6,2.7) ]
        stringify =
            List.map (Tuple.mapBoth String.fromFloat String.fromFloat)
            >> List.map (\(x, y) -> x ++ "," ++ y)
            >> String.join " "
        pointsAstr = stringify pointsA
        pointsBstr = stringify pointsB
        transformStr
            =  "scale(" ++ String.fromFloat s ++ ") "
            ++ "rotate("
                    ++ String.fromFloat r ++ " "
                    ++ String.fromFloat cx ++ " "
                    ++ String.fromFloat cy ++ ")"
    in
        Svg.g
            [ SA.transform transformStr
            ]
            [ Svg.polyline
                [ SA.points pointsAstr
                , SA.fill <| Color.toCssString fill
                ]
                []
            , Svg.polygon
                [ SA.points pointsBstr
                , SA.fill <| Color.toCssString fill
                ]
                []
            ]


{- arrowWithLink : Color -> msg -> Url -> Svg msg
arrowWithLink color onClick url =
    Svg.g
        [ SA.style <| " pointer-events: all; cursor: pointer; transform: translate(" ++
            String.fromFloat gap ++ "px," ++ String.fromFloat gap ++ "px);"
        , onClick onClick
        ]
        [ Svg.a
            [ SA.xlinkHref <| Url.toString url
            , SA.target "_blank"
            ]
            [ Svg.rect
                [ SA.fill "transparent"
                , SA.x "0"
                , SA.y "2.5"
                , SA.width "10"
                , SA.height "10"
                ]
                []
            , arrow color (scale 0.35) (rotate 45)
            ]
        ] -}


-- needed for Safari to properly position sub-groups when VDOM is re-rendered
resetTransform : Svg.Attribute msg
resetTransform = SA.style "transform: translate(0px,0px)"
