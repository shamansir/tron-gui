module RandomGui exposing (generator)


import Random

import Gui.Build as Gui
import Gui.Gui exposing (Gui)
import Gui.Property  exposing (Property)
import Gui.Property as Gui exposing (Label)


generator : Random.Generator (Property ())
generator =
    Random.constant <| Gui.none
