module Tron.Control.Color exposing (..)


import Color exposing (Color)

import Tron.Control as Core exposing (Control)


-- if the color component is being dragged now, we need to know its first value it had when user started dragging,
-- so it is the first `Maybe` in the pair
type alias Control a = Core.Control () (Maybe Color, Color) a
