module Tron.Control.Json.XY exposing (..)


import Tron.Control as Core
import Tron.Control.Impl.XY as XY exposing (Control)

import Json.Decode as D
import Json.Encode as E


decode : D.Decoder (Control ())
decode =
    D.map7
        (\minX maxX stepX minY maxY stepY current ->
            Core.Control
                (
                    { min = minX
                    , max = maxX
                    , step = stepX
                    }
                ,
                    { min = minY
                    , max = maxY
                    , step = stepY
                    }
                )
                ( Nothing, current )
                ()
        )
        (D.field "minX" D.float)
        (D.field "maxX" D.float)
        (D.field "stepX" D.float)
        (D.field "minY" D.float)
        (D.field "maxY" D.float)
        (D.field "stepY" D.float)
        (D.field "current" <|
            D.map2
                Tuple.pair
                (D.field "x" D.float)
                (D.field "y" D.float)
        )


encode : Control a -> List ( String, E.Value )
encode (Core.Control ( xSpec, ySpec ) ( _, ( x, y ) ) _) =
    [ ( "current"
        , E.object
            [ ( "x", E.float x )
            , ( "y", E.float y )
            ]
        )
    , ( "minX", E.float xSpec.min )
    , ( "maxX", E.float xSpec.max )
    , ( "stepX", E.float xSpec.step )
    , ( "minY", E.float ySpec.min )
    , ( "maxY", E.float ySpec.max )
    , ( "stepY", E.float ySpec.step )
    ]


decodeCoord : D.Decoder ( Float, Float )
decodeCoord =
    D.oneOf
        [ D.string
            |> D.andThen
            (\str ->
                case str |> String.split XY.separator of
                    v1 :: v2 :: _ ->
                        Maybe.map2
                            Tuple.pair
                            (String.toFloat v1)
                            (String.toFloat v2)
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault (D.fail <| "failed to parse coord: " ++ str)

                    _ ->
                        D.fail <| "failed to parse coord: " ++ str
            )
        -- FIXME: use either one in the corresponding place
        , D.map2
            Tuple.pair
            (D.field "x" D.float)
            (D.field "y" D.float)
        ]