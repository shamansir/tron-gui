module Constructor.ToBuilder exposing (..)


import Array
import Color exposing (Color)

import Tron exposing (Tron)

import Tron.Tree.Internals exposing (..)
import Tron.Control exposing (..)
import Tron.Control.Value as V
import Tron.Control.Impl.Button as Button
import Tron.Control.Impl.Nest as Nest
import Tron.Style.Theme as Theme
import Tron.Style.PanelShape as PS
import Tron.Style.CellShape as CS


toCodeLines : Tree () -> List String
toCodeLines root =
    [ "module Gui exposing (..)"
    , ""
    , "import Tron.Build.Unit as Tron"
    , "import Tron.Style.CellShape as CS"
    , "import Tron.Style.PanelShape as PS"
    , "import Color"
    , ""
    , "root : Tron ()"
    ] ++ "root = " :: (propToLines root |> indent 4)


propToLines : Tree () -> List String
propToLines prop =
    case prop of

        Nil _ -> [ "Tron.none" ]

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

        Choice _ shape (Control items { form, face, mode } _) ->
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
                ++ modeLines mode


        Live prop_ ->
            propToLines prop_ ++ [ "|> live" ]


labelAndPropToLines : ( String, Tree () ) -> List String
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
formLines form =
    case form of
        Nest.Expanded -> [ "|> Tron.expand" ]
        _ -> []


modeLines : Nest.ChoiceMode -> List String
modeLines mode =
    case mode of
        Nest.Pages -> []
        Nest.Knob -> [ "|> Tron.toKnob" ]
        Nest.SwitchThrough -> [ "|> Tron.toSwitch" ]


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


panelShapeLines : PS.PanelShape -> List String
panelShapeLines ps =
    (case PS.numify ps of
        ( nc, nr ) ->
            if (nc == -1) && (nr == -1) then
                []
            else if (nc == -1) then
                [ "|> Tron.rows " ++ String.fromInt nr ]
            else if (nr == -1) then
                [ "|> Tron.cols " ++ String.fromInt nc ]
            else
                [ "|> Tron.by " ++ String.fromInt nc ++ " " ++ String.fromInt nr ])
    ++ (if not <| PS.pagesEnabled ps then [ ] else [ "|> PS.singlePage " ])


cellShapeLines : CS.CellShape -> List String
cellShapeLines cs  =
    case CS.numify cs of
        ( horz, vert ) ->
            if (horz == 1) && (vert == 1) then
                []
            else [ "|> Tron.cells CS." ++ cellShapeToStr horz vert ++ "" ]


cellShapeToStr : Float -> Float -> String
cellShapeToStr horz vert =
    let
        sideToStr n =
            if n == 1 then "one"
            else if n == 0.5 then "half"
            else if n == 2 then "twice"
            else "unknown"
        sideToStrCap n =
            if n == 1 then "One"
            else if n == 0.5 then "Half"
            else if n == 2 then "Twice"
            else "Unknown"
    in
    if (horz == 0.5) && (vert == 0.5) then
        "half"
    else sideToStr horz ++ "By" ++ sideToStrCap vert


shapeLines : NestShape -> List String
shapeLines ( panelShape, cellShape ) =
    panelShapeLines panelShape ++ cellShapeLines cellShape


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