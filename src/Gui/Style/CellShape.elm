module Gui.Style.CellShape exposing
    ( CellShape
    , default
    , single, half, halfByOne, oneByHalf, twiceByHalf, halfByTwice, twiceByTwice
    , isHorizontal, isVertical, isSquare, isLargeSquare
    , numify, toString
    )

{-| # Cell Shape

Cell Shape is the place it takes in nested panels. Considering the default shape as 1x1 (`Full`), the meaning of each value is:

* `single` — 1x1
* `half` — 0.5x0.5
* `halfByOne` — 0.5x1
* `oneByHalf` — 1x0.5
* `twiceByHalf` — 2x0.5
* `halfByTwice` — 0.5x2
* `twiceByTwice` - 2x2

@docs CellShape

# Values
@docs single, half, halfByOne, oneByHalf, twiceByHalf, halfByTwice, twiceByTwice

# Helpers
@docs numify, toString
-}


type Unit
    = Single
    | Half
    | Twice


{-|
-}
type CellShape = CellShape Unit Unit


{-|
-}
default : CellShape
default = single


{-|
-}
single : CellShape
single = CellShape Single Single


{-|
-}
half : CellShape
half = CellShape Half Half


{-|
-}
halfByOne : CellShape
halfByOne = CellShape Half Single


{-|
-}
oneByHalf : CellShape
oneByHalf = CellShape Single Half


{-|
-}
twiceByHalf : CellShape
twiceByHalf = CellShape Twice Half


{-|
-}
halfByTwice : CellShape
halfByTwice = CellShape Half Twice


{-|
-}
twiceByTwice : CellShape
twiceByTwice = CellShape Twice Twice


numifyUnit : Unit -> Float
numifyUnit u =
    case u of
        Single -> 1.0
        Half -> 0.5
        Twice -> 2.0


{-|
-}
numify : CellShape -> ( Float, Float )
numify cs =
    case cs of
        CellShape xu yu ->
            ( numifyUnit xu, numifyUnit yu )



{-|
-}
toString : CellShape -> String
toString cs =
    let
        unitToString u =
            case u of
                Single -> "single"
                Half -> "half"
                Twice -> "twice"
    in
    case cs of
        CellShape rn cn ->
            if rn == cn then
                unitToString rn
            else
                unitToString rn ++ "-by-" ++ unitToString cn


isSquare : CellShape -> Bool
isSquare cs =
    case cs of
        CellShape Single Single -> True
        CellShape Twice Twice -> True
        _ -> False


isSmallSquare : CellShape -> Bool
isSmallSquare cs =
    case cs of
        CellShape Single Single -> True
        _ -> False


isLargeSquare : CellShape -> Bool
isLargeSquare cs =
    case cs of
        CellShape Twice Twice -> True
        _ -> False


isHorizontal : CellShape -> Bool
isHorizontal cs =
    case cs of
        CellShape Twice Half -> True
        _ -> False


isVertical : CellShape -> Bool
isVertical cs =
    case cs of
        CellShape Half Twice -> True
        _ -> False
