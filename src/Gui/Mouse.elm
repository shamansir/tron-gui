module Gui.Mouse exposing (..)


import Json.Decode as D


type alias Position = { x: Int, y : Int }


type MouseAction
    = Up Position
    | Down Position
    | Move Position


type alias MouseState =
    { pos: Position
    , down : Bool
    , dragFrom : Maybe Position
    }


apply : MouseAction -> MouseState -> MouseState
apply action prev =
    case action of
        Move pos ->
            { prev
            | pos = pos
            }
        Up pos ->
            { prev
            | pos = pos
            , down = False
            , dragFrom = Nothing
            }
        Down pos ->
            { prev
            | pos = pos
            , down = True
            , dragFrom = Just pos
            }


init : MouseState
init =
    { pos = { x=0, y=0 }
    , down = False
    , dragFrom = Nothing
    }


subPos : Position -> Position -> Position
subPos toSub prev =
    { x = prev.x - toSub.x
    , y = prev.y - toSub.y
    }


addPos : Position -> Position -> Position
addPos toAdd prev =
    { x = prev.x + toAdd.x
    , y = prev.y + toAdd.y
    }


shift : Position -> MouseState -> MouseState
shift pos prev =
    { prev
    | pos = subPos pos prev.pos
    }


decodePosition : D.Decoder Position
decodePosition =
    D.map2 Position
        (D.at [ "clientX" ] D.int)
        (D.at [ "clientY" ] D.int)
