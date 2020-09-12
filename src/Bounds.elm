module Bounds exposing (..)


type alias Bounds =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


zero : Bounds
zero =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    }


multiply : { x : Float, y : Float } -> Bounds -> Bounds
multiply mult bounds =
    { x = mult.x * bounds.x
    , y = mult.y * bounds.y
    , width = mult.x * bounds.width
    , height = mult.y * bounds.height
    }


multiplyBy : Float -> Bounds -> Bounds
multiplyBy n =
    multiply { x = n, y = n }


shift : { a | x : Float, y : Float } -> Bounds -> Bounds
shift by bounds =
    { x = bounds.x + by.x
    , y = bounds.y + by.y
    , width = bounds.width
    , height = bounds.height
    }
