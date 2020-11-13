module Gui.Style.Tone exposing (..)

import Color
import Color exposing (Color)
import Gui.Focus exposing (Focused)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Theme as Theme exposing (..)
import Gui.Style.Placement exposing (Placement)


green = Color.rgb255 0 204 71 -- "#00cc47"


pink = Color.rgb255 237 49 162 -- "#ED31A2"


yellow = Color.rgb255 234 176 0 -- "#eab000"


aqua = Color.rgb255 35 205 232 -- "#23CDE8"


black = Color.rgb255 58 62 65 -- "#3a3e41"



type Tone
    = None
    | Green
    | Pink
    | Yellow
    | Aqua


lines : Theme -> Tone -> Color
lines theme tone =
    case tone of
        Green -> green
        Pink -> pink
        Yellow -> yellow
        Aqua -> aqua
        None -> Theme.lines theme


secondaryLines : Theme -> Tone -> Color
secondaryLines theme _ =
    Theme.secondaryLines theme


text : Theme -> Tone -> Color
text theme _ =
    Theme.text theme


textHilite : Theme -> Tone -> Color
textHilite theme _ =
    Theme.textHilite theme


focusBack : Theme -> Tone -> Placement -> Focused -> Color
focusBack theme tone placement =
    Theme.focusBack theme placement


toString : Tone -> String
toString tone =
    case tone of
        Green -> "tone-1"
        Pink -> "tone-2"
        Yellow -> "tone-3"
        Aqua -> "tone-4"
        None -> "no-tone"


next : Tone -> Tone
next style =
    case style of
        Green -> Pink
        Pink -> Yellow
        Yellow -> Aqua
        Aqua -> Green
        None -> None
