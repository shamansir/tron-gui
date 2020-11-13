module Gui.Style.CellShape exposing
    ( CellShape
    , single, half, halfByOne, oneByHalf, twiceByHalf, halfByTwice, twiceByTwice
    , numify
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
