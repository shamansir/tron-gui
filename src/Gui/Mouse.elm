module Gui.Mouse exposing (..)


import Json.Decode as D


type alias Position = { x: Int, y : Int }


type alias MouseState =
    { pos: Position
    , down : Bool
    , dragFrom : Maybe Position
    }


init : MouseState
init =
    { pos = { x=0, y=0 }
    , down = False
    , dragFrom = Nothing
    }


moves : Position -> MouseState -> MouseState
moves pos prev =
    { prev
    | pos = pos
    }


ups : a -> MouseState -> MouseState
ups _ prev =
    { prev
    | down = False
    , dragFrom = Nothing
    }


downs : Position -> MouseState -> MouseState
downs pos prev =
    { prev
    | pos = pos
    , down = True
    , dragFrom =
        case prev.dragFrom of
            Just prevDragPos -> Just prevDragPos
            Nothing -> Just pos
    }


subPos : Position -> Position -> Position
subPos toSub prev =
    { x = prev.x - toSub.x
    , y = prev.y - toSub.y
    }


shift : Position -> MouseState -> MouseState
shift pos prev =
    { prev
    | pos = subPos pos prev.pos
    }


decodePosition : D.Decoder Position
decodePosition =
    D.map2 Position
        (D.at [ "offsetX" ] D.int)
        (D.at [ "offsetY" ] D.int)
