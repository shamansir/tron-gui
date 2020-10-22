module Gui.Render.Style exposing (..)


import Dict exposing (Dict)


import Color exposing (Color)

import Gui.Property as Property exposing (Property(..))
import Gui.Path as Path exposing (Path, toString)
import Gui.Focus exposing (Focused(..))


type Theme
    = Dark
    | Light


type Tone
    = None
    | Green
    | Pink
    | Yellow
    | Aqua


green = Color.rgb255 0 204 71 -- "#00cc47"


pink = Color.rgb255 237 49 162 -- "#ED31A2"


yellow = Color.rgb255 234 176 0 -- "#eab000"


aqua = Color.rgb255 35 205 232 -- "#23CDE8"


black = Color.rgb255 58 62 65 -- "#3a3e41"


white = Color.white


gray = Color.rgb255 220 220 220 -- "#eeeeee"


gray2 = Color.rgba 1.0 1.0 1.0 0.15


label = Color.rgb255 144 144 144 -- "#909090"


lightBackground = Color.rgba 1.0 1.0 1.0 0.8


darkBackground = Color.rgba 0.05 0.05 0.05 0.6


canvasBackground = Color.lightGray


transparent = Color.rgba 0.0 0.0 0.0 0.0


cellWidth : Float
cellWidth = 90


cellHeight : Float
cellHeight = 90


gap = 10


borderRadius = 10


fontFamily = "\"IBM Plex Sans\", sans-serif"


colorFor : Theme -> Tone -> Color
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


text : Theme -> Color
text _ =
    Color.rgb255 144 144 144

knobLine : Theme -> Color
knobLine theme =
    case theme of
        Dark -> gray2
        Light -> gray


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


focusColor : Theme -> Focused -> Maybe Color
focusColor theme focused =
    case focused of
        NotFocused -> Nothing
        FocusedBy n ->
            Just <| case theme of
                Light ->
                    Color.rgb 1.0 1.0 1.0
                Dark ->
                    Color.rgb 0.2 0.2 0.2
                {-
                if n == 0 then rgb 1.0 1.0 1.0
                else if n == 1 then rgb 0.95 0.95 0.95
                else ... -}
