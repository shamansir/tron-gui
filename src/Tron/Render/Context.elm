module Tron.Render.Context exposing (..)


import Bounds exposing (BoundsF)


import Tron.Path as Path exposing (Path)
import Tron.Style.Selected exposing (Selected)
import Tron.Style.Placement exposing (Placement)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Focus exposing (Focused)


type alias Context =
    { placement : Placement
    , focused : Focused
    , selected : Selected
    --, path : Path
    , bounds : BoundsF
    , cellShape : CellShape
    --, maybeSelectedInside : Maybe ( Path.Label, Tree a )
    }


type alias StyleDef = ( Placement, Focused, Selected )


styleDef : Context -> StyleDef
styleDef { placement, focused, selected } = ( placement, focused, selected )