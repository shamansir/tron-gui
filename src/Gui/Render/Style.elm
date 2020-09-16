module Gui.Render.Style exposing (..)


type alias Color = String


type Mode
    = Dark
    | Light


type StyleColor = StyleColor Color


type Style
    = Green
    | Pink
    | Yellow
    | Aqua
    | Black


green = StyleColor "#00cc47"


pink = StyleColor "#ED31A2"


yellow = StyleColor "#eab000"


aqua = StyleColor "#23CDE8"


black = StyleColor "#3a3e41"


white = StyleColor "#ffffff"


gray = "#eeeeee"


gray2 = "rgb(255 255 255 / 0.15)"


label = "#909090"


lightBackground = "rgb(255 255 255 / 80%)"


darkBackground = "rgb(15 15 15 / 60%)"


canvasBackground = "lightgray"


colorFor : Style -> StyleColor
colorFor style =
    case style of
        Green -> green
        Pink -> pink
        Yellow -> yellow
        Aqua -> aqua
        Black -> black


next : Style -> Style
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


colorToString : StyleColor -> String
colorToString (StyleColor s) = s
