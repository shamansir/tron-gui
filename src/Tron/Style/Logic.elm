module Tron.Style.Logic exposing (..)


import Dict exposing (Dict)

import Color exposing (Color)
import BinPack exposing (Bounds)

import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CellShape exposing (toString)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Cell as Cell


import Tron.Mouse exposing (Position)
import Tron.Property as Property exposing (Property(..))
import Tron.Path as Path exposing (Path, toString)
import Tron.Focus exposing (Focused(..))




-- white = Color.white


-- label = Color.rgb255 144 144 144 -- "#909090"



-- canvasBackground = Color.lightGray


-- transparent = Color.rgba 0.0 0.0 0.0 0.0



-- TODO: make bounds to be bounded to pariticular units
toGridCoords : Bounds -> Position -> Position
toGridCoords bounds pos =
    { x = (pos.x - bounds.x) / Cell.width
    , y = (pos.y - bounds.y) / Cell.height
    }


shapeToModifier : CellShape -> String
shapeToModifier =
    CellShape.toString
