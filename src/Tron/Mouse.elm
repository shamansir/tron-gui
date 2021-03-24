module Tron.Mouse exposing (..)


import Tron.Util exposing (align)

import Json.Decode as D


type alias Position = { x: Float, y : Float }


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


adapt : (Position -> Position) -> MouseState -> MouseState
adapt f mstate =
    { pos = f mstate.pos
    , down = mstate.down
    , dragFrom = mstate.dragFrom |> Maybe.map f
    }


{-
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
-}


decodePosition : D.Decoder Position
decodePosition =
    D.map2 Position
        (D.at [ "clientX" ] D.float)
        (D.at [ "clientY" ] D.float)


distanceY : Float -> MouseState -> Float
distanceY howFar mstate  =
    case mstate.dragFrom of
        Just dragFrom ->
            if mstate.pos /= dragFrom then
                let
                    originY = dragFrom.y
                    curY = mstate.pos.y
                    topY = originY + (howFar / 2)
                    diffY = (topY - curY) / howFar
                in
                    align diffY
            else 0
        _ -> 0


distanceXY : Float -> MouseState -> ( Float, Float )
distanceXY howFar mstate  =
    case mstate.dragFrom of
        Just dragFrom ->
            if mstate.pos /= dragFrom then
                let
                    originX = dragFrom.x
                    originY = dragFrom.y
                    curX = mstate.pos.x
                    curY = mstate.pos.y
                    leftX = originX - (howFar / 2)
                    -- Y is going from top to bottom
                    topY = originY + (howFar / 2)
                    diffX = (curX - leftX) / howFar
                    diffY = (topY - curY) / howFar
                in
                    ( align diffX
                    , align (1 - diffY)
                    )
            else ( 0, 0 )
        _ -> ( 0, 0 )

