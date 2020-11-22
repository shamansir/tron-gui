module Example.Goose.Gui exposing (..)


import Gui exposing (Gui)
import Gui.Build as Gui

import Example.Goose.Model exposing (..)
import Example.Goose.Msg exposing (..)


for : Model -> Gui.Builder Msg
for _ = Gui.none
