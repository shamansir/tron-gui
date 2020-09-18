module Gui.Render.Style exposing (..)


import Dict exposing (Dict)

import Gui.Property as Property exposing (Property(..))
import Gui.Path as Path exposing (Path, toString)


type alias Color = String


type Mode
    = Dark
    | Light


type ToneColor = ToneColor Color


type Tone
    = Green
    | Pink
    | Yellow
    | Aqua
    | Black


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


colorFor : Tone -> ToneColor
colorFor style =
    case style of
        Green -> green
        Pink -> pink
        Yellow -> yellow
        Aqua -> aqua
        Black -> black


next : Tone -> Tone
next style =
    case style of
        Green -> Pink
        Pink -> Yellow
        Yellow -> Aqua
        Aqua -> Green
        Black -> Black


background : Mode -> Color
background mode =
    case mode of
        Dark -> darkBackground
        Light -> lightBackground


knobLine : Mode -> Color
knobLine mode =
    case mode of
        Dark -> gray2
        Light -> gray


toneToString : ToneColor -> String
toneToString (ToneColor s) = s



assignTones : Property msg -> Dict String Tone -- List ( Path, Tone )
assignTones =
    let
        assignTone ( path, prop ) ( knownTones, nextTone ) =
            if Path.howDeep path == 0 then
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
                        ( knownTones |> Dict.insert (Path.toString path) Black
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
                        ( knownTones |> Dict.insert (Path.toString path) Black
                        , nextTone
                        )

    in
    Property.unfold
        >> List.sortBy (Tuple.first >> Path.howDeep)
        >> List.foldl assignTone ( Dict.empty, Green )
        >> Tuple.first
        -->> List.map (Tuple.mapFirst Gui.Path.toString)


