module Constructor.ToBuilder exposing (..)


import Array
import Color exposing (Color)

import Tron exposing (Tron)

import Tron.Property exposing (..)
import Tron.Control exposing (..)
import Tron.Control.Value as V
import Tron.Control.Button as Button
import Tron.Control.Nest as Nest
import Tron.Style.Theme as Theme


toCodeLines : Tron () -> List String
toCodeLines root =
    [ "module Gui exposing (..)"
    , ""
    , "import Tron.Builder.Unit as Tron"
    , "import Color"
    , ""
    , "root : Tron ()"
    ] ++ "root = " :: (propToLines root |> indent 4)


propToLines : Tron () -> List String
propToLines prop =
    case prop of

        Nil -> [ "Tron.none" ]

        Number (Control { min, max, step } ( _, value ) _) ->
            [
                "Tron.number"
                , "    { min = " ++ String.fromFloat min
                , "    , max = " ++ String.fromFloat max
                , "    , step = " ++ String.fromFloat step
                , "    }"
                , "    " ++ String.fromFloat value
            ]

        Coordinate (Control (axisX, axisY) ( _, (valueX, valueY) ) _) ->
            [
                "Tron.xy"
                , "    ("
                , "        { min = " ++ String.fromFloat axisX.min
                , "        , max = " ++ String.fromFloat axisX.max
                , "        , step = " ++ String.fromFloat axisX.step
                , "        }"
                , "    ,"
                , "        { min = " ++ String.fromFloat axisY.min
                , "        , max = " ++ String.fromFloat axisY.max
                , "        , step = " ++ String.fromFloat axisY.step
                , "        }"
                , "    )"
                , "    ( " ++ String.fromFloat valueX ++ ", " ++ String.fromFloat valueY ++ " )"
            ]

        Text (Control _ (  _, value ) _) ->
            [
                "Tron.text \"" ++ value ++ "\""
            ]

        Color (Control _ (  _, value ) _) ->
            case Color.toRgba value of
                { red, green, blue, alpha } ->
                    [
                        "Tron.color"
                        , "    <| Color.fromRgba"
                        , "        { red = " ++ String.fromFloat red
                        , "        , green = " ++ String.fromFloat green
                        , "        , blue = " ++ String.fromFloat blue
                        , "        , alpha = " ++ String.fromFloat alpha
                        , "        }"
                    ]

        Toggle (Control _ value _) ->
            [
                "Tron.toggle "
                    ++ if V.toggleToBool value then "True" else "False"
            ]

        Action (Control face _ _) ->
            "Tron.button"
            :: faceLines (Just face)

        Group _ shape (Control items { form, face } _) ->
            "Tron.nest"
                :: (list labelAndPropToLines (Array.toList items) |> indent 4)
                ++ shapeLines shape
                ++ faceLines face
                ++ formLines form

        Choice _ shape (Control items { form, face } _) ->
            "Tron.choice"
                :: (list labelAndPropToLines (Array.toList items) |> indent 4)
                ++ ([
                     Array.toList items
                     |> List.head
                     |> Maybe.map Tuple.first
                     |> Maybe.map quote
                     |> Maybe.withDefault "-"
                    ] |> indent 4)
                ++ shapeLines shape
                ++ faceLines face
                ++ formLines form

        Live prop_ ->
            propToLines prop_ ++ [ "|> live" ]


labelAndPropToLines : ( String, Tron () ) -> List String
labelAndPropToLines ( label, prop ) =
    [ "( " ++ quote label
    , "  , " ]
    ++ (propToLines prop |> indent 4)
    ++ [ "  )" ]


addComma : List String -> List String
addComma strings =
    case strings of
        [] -> strings
        first_::rest_
            -> (", " ++ first_) :: rest_


list : (a -> List String) -> List a -> List String
list f l =
    case l of
        [] -> [ "[", "]" ]
        first::rest ->
            ("[ " ++ (List.head (f first) |> Maybe.withDefault ""))
            :: (List.tail (f first) |> Maybe.withDefault [])
            ++ (rest |> List.map f |> List.map addComma |> List.concat)
            ++ [ "]" ]


indent : Int -> List String -> List String
indent n strings =
    strings
        |> List.map2 (++)
            (List.repeat (List.length strings) (List.repeat n " " |> String.join ""))



formLines : Nest.Form -> List String
formLines form = []


faceLines : Maybe Button.Face -> List String
faceLines face =
    case face of
        Just Button.Default -> []
        Just (Button.WithIcon (Button.Icon fn)) ->
            [ "|> Tron.face"
            , "    (Tron.iconAt " ++ iconUrlToString (fn Theme.Dark) ++ ")" ]
        Just (Button.WithColor color) ->
            [ "|> Tron.face"
            , "    (Tron.useColor <| Color.fromRgba " ]
            ++ (colorLines (Color.toRgba color) |> indent 8)
            ++ [ "    )" ]
        Nothing -> []


shapeLines : NestShape -> List String
shapeLines ( panelShape, cellShape ) = []


colorLines : { red : Float, blue : Float, green : Float, alpha : Float } -> List String
colorLines { red, green, blue, alpha } =
    [ "{ red = " ++ String.fromFloat red
    , ", green = " ++ String.fromFloat green
    , ", blue = " ++ String.fromFloat blue
    , ", alpha = " ++ String.fromFloat alpha
    , "}"
    ]

quote : String -> String
quote str =
    "\"" ++ str ++ "\""


iconUrlToString : Button.Url -> String
iconUrlToString url =
    case url of
        Button.Url theUrl ->
            "["
            ++ (theUrl
                |> String.split "/"
                |> List.map quote
                |> List.intersperse " , "
                |> String.join "")
            ++ "]"