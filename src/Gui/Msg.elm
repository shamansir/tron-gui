module Gui.Msg exposing (..)


import Gui.Def exposing (..)
import Gui.Mouse exposing (..)
import Gui.Grid exposing (..)


type Msg umsg
    = NoOp
    | ApplyMouse MouseAction -- from the document
    | Click (GridCell umsg) -- specific cell
    | MouseDown (GridCell umsg) -- specific cell
    | KeyDown Int (Maybe (GridCell umsg)) -- specific cell
    | FocusOn NestPos
    | Tune NestPos AlterKnob
    | ToggleOn NestPos
    | ToggleOff NestPos
    | ExpandNested NestPos
    | CollapseNested NestPos
    | ExpandChoice NestPos
    | CollapseChoice NestPos
    | Select NestPos
    -- | Move NestPos Int
    | ShiftFocusLeftAt NestPos
    | ShiftFocusRightAt NestPos
    -- | SendToUser umsg
    -- | SelectAndSendToUser NestPos umsg
    -- | ToggleOnAndSendToUser NestPos umsg
    -- | ToggleOffAndSendToUser NestPos umsg
    -- | TuneAndApply NestPos AlterKnob umsg

    -- | Color
