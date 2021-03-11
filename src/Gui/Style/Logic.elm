module Gui.Style.Logic exposing (..)


import Dict exposing (Dict)

import Color exposing (Color)
import BinPack exposing (Bounds)

import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CellShape exposing (toString)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Cell as Cell


import Gui.Mouse exposing (Position)
import Gui.Property as Property exposing (Property(..))
import Gui.Path as Path exposing (Path, toString)
import Gui.Focus exposing (Focused(..))




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
