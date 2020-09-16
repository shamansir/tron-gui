module Gui.Render.Style exposing (..)


type StyleColor = StyleColor String


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


gray = "#eeeeee"


label = "#909090"


lightBackground = "rgb(255 255 255 / 80%)"


darkBackground = "rgb(15 15 15 / 60%)"


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
