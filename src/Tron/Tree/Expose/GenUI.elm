module Tron.Tree.Expose.GenUI exposing (..)


import GenUI exposing (GenUI)

import Tron.Tree.Internals as Tree exposing (..)

to : Tree a -> GenUI
to _ =
    { version = GenUI.version
    , root = []
    }


from : GenUI -> Result String (Tree ())
from _ = Err "failed"