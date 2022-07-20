module Color.Extra exposing (fromHex, toHex)

import Bitwise exposing (shiftLeftBy)
import Color exposing (Color)

{- a copy from the source code from avh4/elm-color, these functions are just no being exposed. See https://github.com/avh4/elm-color/pull/24 -}


fromHex : String -> Maybe Color
fromHex hexString =
    case String.toList hexString of
        [ '#', r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ '#', r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ '#', r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ '#', r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        _ ->
            Nothing


fromHex8 : ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Maybe Color
fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    Maybe.map4
        (\r g b a ->
            Color.rgba
                (toFloat r / 255)
                (toFloat g / 255)
                (toFloat b / 255)
                (toFloat a / 255)
        )
        (hex2ToInt r1 r2)
        (hex2ToInt g1 g2)
        (hex2ToInt b1 b2)
        (hex2ToInt a1 a2)


hex2ToInt : Char -> Char -> Maybe Int
hex2ToInt c1 c2 =
    Maybe.map2 (\v1 v2 -> shiftLeftBy 4 v1 + v2) (hexToInt c1) (hexToInt c2)


hexToInt : Char -> Maybe Int
hexToInt char =
    case Char.toLower char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


{-| This function will convert a color to a 6-digit hexadecimal string in the format `#rrggbb`.

NOTE: If you want to use the resulting string with CSS, you should instead use [`toCssString`](#toCssString),
which will represent the color more accurately, and preserve the alpha component.

-}
toHex : Color -> { hex : String, alpha : Float }
toHex c =
    let
        components =
            Color.toRgba c
    in
    { hex =
        [ components.red, components.green, components.blue ]
            |> List.map ((*) 255)
            |> List.map round
            |> List.map int255ToHex
            |> String.concat
            |> (++) "#"
    , alpha = components.alpha
    }


int255ToHex : Int -> String
int255ToHex n =
    if n < 0 then
        "00"

    else if n > 255 then
        "ff"

    else
        unsafeInt255Digits n
            |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
            |> (\( a, b ) -> String.cons a (String.cons b ""))


unsafeInt255Digits : Int -> ( Int, Int )
unsafeInt255Digits n =
    let
        digit1 =
            n // 16

        digit0 =
            if digit1 /= 0 then
                modBy (digit1 * 16) n

            else
                n
    in
    ( digit1, digit0 )


unsafeIntToChar : Int -> Char
unsafeIntToChar i =
    if i < 10 then
        String.fromInt i
            |> String.uncons
            |> Maybe.map Tuple.first
            |> Maybe.withDefault '0'

    else
        case i of
            10 ->
                'a'

            11 ->
                'b'

            12 ->
                'c'

            13 ->
                'd'

            14 ->
                'e'

            15 ->
                'f'

            _ ->
                '0'