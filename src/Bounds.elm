module Bounds exposing (..)


type alias Bounds =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias BoundsF =
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


multiply : { x : Int, y : Int } -> Bounds -> Bounds
multiply mult bounds =
    { x = mult.x * bounds.x
    , y = mult.y * bounds.y
    , width = mult.x * bounds.width
    , height = mult.y * bounds.height
    }


multiplyBy : Int -> Bounds -> Bounds
multiplyBy n =
    multiply { x = n, y = n }


shift : { a | x : Int, y : Int } -> Bounds -> Bounds
shift by bounds =
    { x = bounds.x + by.x
    , y = bounds.y + by.y
    , width = bounds.width
    , height = bounds.height
    }


toFloat : Bounds -> BoundsF
toFloat bounds =
    { x = Basics.toFloat bounds.x
    , y = Basics.toFloat bounds.y
    , width = Basics.toFloat bounds.width
    , height = Basics.toFloat bounds.height
    }


divide : { x : Float, y : Float } -> BoundsF -> BoundsF
divide div bounds =
    { x = bounds.x / div.x
    , y = bounds.y / div.y
    , width = bounds.width / div.x
    , height = bounds.height / div.y
    }


divideBy : Float -> BoundsF -> BoundsF
divideBy n = divide { x = n, y = n }

