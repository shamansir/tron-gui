module Tron.Style.Dock exposing
    ( Dock, HorzAnchor(..), VertAnchor(..)
    , topLeft, topCenter, topRight
    , middleLeft, center, middleRight
    , bottomLeft, bottomCenter, bottomRight
    , anchors, horzAnchor, vertAnchor
    , toString
    )

{-| # Dock
@docs Dock

# Values
@docs topLeft, topCenter, topRight
@docs middleLeft, center, middleRight
@docs bottomLeft, bottomCenter, bottomRight

# Anchors
@docs HorzAnchor, VertAnchor, anchors, horzAnchor, vertAnchor

# Helpers
@docs toString
-}

import Tron.Style.Cell as Cell
import SmartPack as D exposing (Distribution(..))

import Size exposing (..)


{-| -}
type HorzAnchor
    = Left
    | Center
    | Right


{-| -}
type VertAnchor
    = Top
    | Middle
    | Bottom


{-| `Dock` describes the direction in which GUI is oriented and to which corner or side of the page it is "docked".

If you are familiar with macOS Dock — here we have the similar concept.
-}
type Dock = Dock ( HorzAnchor, VertAnchor )

{-|
-}
topLeft : Dock
topLeft = Dock ( Left, Top )


{-|
-}
topCenter : Dock
topCenter = Dock ( Center, Top )


{-|
-}
topRight : Dock
topRight = Dock ( Right, Top )


{-|
-}
middleLeft : Dock
middleLeft = Dock ( Left, Middle )


{-|
-}
center : Dock
center = Dock ( Center, Middle )


{-|
-}
middleRight : Dock
middleRight = Dock ( Right, Middle )


{-|
-}
bottomLeft : Dock
bottomLeft = Dock ( Left, Bottom )


{-|
-}
bottomCenter : Dock
bottomCenter = Dock ( Center, Bottom )


{-|
-}
bottomRight : Dock
bottomRight = Dock ( Right, Bottom )


{-| -}
anchors : Dock -> ( HorzAnchor, VertAnchor )
anchors (Dock anchors_) = anchors_


{-| -}
horzAnchor : Dock -> HorzAnchor
horzAnchor = anchors >> Tuple.first


{-| -}
vertAnchor : Dock -> VertAnchor
vertAnchor = anchors >> Tuple.second



{-| -}
toString : Dock -> String
toString (Dock ( horz, vert )) =
    ( case horz of
        Left -> "left"
        Center -> "center"
        Right -> "right"
    ) ++ "-" ++
    ( case vert of
        Top -> "top"
        Middle -> "middle"
        Bottom -> "bottom"
    )
