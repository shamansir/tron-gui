module Tron.Tree.Expose.GenUI exposing (to, from)


import GenUI exposing (GenUI)

import Tron.Tree.Internals as Tree exposing (..)

import Tron.Control.GenUI.Button as Button


treeToGenUI : Tree a -> GenUI.Property
treeToGenUI t =
    let
        makeProp name def =
            { name = name
            , shape = Nothing
            , def = def
            , live = False
            , property = Nothing
            }
    in case t of
        Nil _ -> GenUI.root -- FIXME
        Action button -> makeProp "button" <| Button.to button
        _ -> GenUI.root -- FIXME
        -- Number def -> makeProp "num" <|


to : Tree a -> GenUI
to t =
    { version = GenUI.version
    , root = [ treeToGenUI t ]
    }


from : GenUI -> Result String (Tree ())
from _ = Err "failed"