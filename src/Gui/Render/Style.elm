module Gui.Render.Style exposing (..)


import Dict exposing (Dict)

import Gui.Property as Property exposing (Property(..))
import Gui.Path as Path exposing (Path, toString)


type alias Color = String


type Theme
    = Dark
    | Light


type ToneColor = ToneColor Color


type Tone
    = None
    | Green
    | Pink
    | Yellow
    | Aqua


green = ToneColor "#00cc47"


pink = ToneColor "#ED31A2"


yellow = ToneColor "#eab000"


aqua = ToneColor "#23CDE8"


black = ToneColor "#3a3e41"


white = ToneColor "#ffffff"


gray = "#eeeeee"


gray2 = "rgb(255 255 255 / 0.15)"


label = "#909090"


lightBackground = "rgb(255 255 255 / 80%)"


darkBackground = "rgb(15 15 15 / 60%)"


canvasBackground = "lightgray"


colorFor : Theme -> Tone -> ToneColor
colorFor theme style =
    case style of
        Green -> green
        Pink -> pink
        Yellow -> yellow
        Aqua -> aqua
        None ->
            case theme of
                Dark -> white
                Light -> black


next : Tone -> Tone
next style =
    case style of
        Green -> Pink
        Pink -> Yellow
        Yellow -> Aqua
        Aqua -> Green
        None -> None


background : Theme -> Color
background theme =
    case theme of
        Dark -> darkBackground
        Light -> lightBackground


knobLine : Theme -> Color
knobLine theme =
    case theme of
        Dark -> gray2
        Light -> gray


toneToString : ToneColor -> String
toneToString (ToneColor s) = s


assignTones : Property msg -> Dict String Tone -- List ( Path, Tone )
assignTones =
    let
        assignTone ( path, prop ) ( knownTones, nextTone ) =
            if Path.howDeep path == 1 then
                case prop of
                    Group _ ->
                        ( knownTones |> Dict.insert (Path.toString path) nextTone
                        , next nextTone
                        )
                    Choice _ ->
                        ( knownTones |> Dict.insert (Path.toString path) nextTone
                        , next nextTone
                        )
                    _ ->
                        ( knownTones |> Dict.insert (Path.toString path) None
                        , nextTone
                        )
            else -- path is deeper than 0
                case Path.head path of
                    Just sourcePath ->
                        let
                            parentTone =
                                knownTones
                                    |> Dict.get (sourcePath |> Path.toString)
                                    -- |> Dict.get (Path.retract path |> Path.toString)
                                    |> Maybe.withDefault nextTone

                        in
                            ( knownTones |> Dict.insert (Path.toString path) parentTone
                            , nextTone
                            )
                    Nothing ->
                        ( knownTones |> Dict.insert (Path.toString path) None
                        , nextTone
                        )

    in
    Property.unfold
        >> List.sortBy (Tuple.first >> Path.howDeep)
        >> List.foldl assignTone ( Dict.empty, Green )
        >> Tuple.first
        -->> List.map (Tuple.mapFirst Gui.Path.toString)


switch : Theme -> Theme
switch from =
    case from of
        Dark -> Light
        Light -> Dark
