module Tron.Tree exposing
    ( Tree
    , empty
    , get, getValue
    , map, mapWithPath, mapWithValue
    , zip
    , andThen, with
    , toUnit, proxify, pathify
    , fold, unfold
    , apply
    )

{-| The `Tree a` is the structure of the components of your GUI. Also, it stores some `a` subject with every control.

You get the `Tree ()` as the previous state of controls in `for` and other functions like `update` and `view`, when you use `WithTron` helpers,
so that you may get their previos values using `WithTron.ValueAt`.

`Tron msg`, on the other side, is just `Tree (Control.Value -> Maybe msg)`, so every control strores the handler that convers its value to the messages.
It is the structure you return from `for` function so that the system knows what messages to produce in response.

# Types

@docs Tree

# Empty

@docs empty

# Mappings, zips & folds

@docs map, mapWithPath, mapWithValue, zip, fold, unfold

# Bindings

@docs andThen, with

# Convert

@docs toUnit, proxify, pathify

# Get value

@docs get, getValue

# Apply value

@docs apply
-}


import Tron.Path exposing (Path)
import Tron.Control.Value as Control

import Tron.Tree.Internals as I
import Tron.Tree.Paths as I


{-| -}
type alias Tree a = I.Tree a


{-| Create the empty tree. Has to store something though, let it be `()`. -}
empty : Tree ()
empty = I.empty


{-| -}
map : (a -> b) -> Tree a -> Tree b
map = I.map


{-| -}
mapWithPath
    :  (Path -> a -> b)
    -> Tree a
    -> Tree b
mapWithPath =
    I.mapWithPath



{-| -}
mapWithValue
    :  (Path -> Control.Value -> a -> b)
    -> Tree a
    -> Tree b
mapWithValue =
    I.mapWithValue


{-| -}
andThen : (a -> Tree b) -> Tree a -> Tree b
andThen = I.andThen
--andThen f = foldP <| always (get >> f)


{-| Same as `andThen`, but also passes the same current property to the function. It helps in chaining things in `Tree.Build.*` and `Tron.Build`. -}
with : (a -> Tree a -> Tree b) -> Tree a -> Tree b
-- FIXME: should be changed to `andThen` with getting rid of function in Control
with = I.with


{-| Convert the tree to the full plane list of its components. So it goes as deep as there is some non-nested component and adds it to the list as well as its parent. -}
unfold : Tree a -> List (Path, Tree a)
unfold = I.unfold


{-| Goes through all the inner components of the Tree (as deep as there is some non-nested component) and calls the function for every of those, folding them into one structure. -}
fold : (Path -> Tree a -> b -> b) -> b -> Tree a -> b
fold = I.fold


{-| Get the subject of the top-level component. -}
get : Tree a -> a
get = I.get


{-| Replace the subject everywhere within Tron GUI tree with `()`, it is useful for truly a lot of cases when you don't care about what are the associated values.
-}
toUnit : Tree a -> Tree ()
toUnit = I.toUnit


{-| Replace the subject everywhere within Tron GUI tree with the associated `Control.Value`.
-}
proxify : Tree a -> Tree Control.Value
proxify = I.proxify


{-| Replace the subject everywhere within Tron GUI tree with the associated `Control.Value`.
-}
pathify : Tree a -> Tree Path
pathify = I.pathify


{-| Take the `Tree` with a handler and call this handler for every control, storing the produced `a` as the subject.
Usually, it is done to calculate `msg` at this place, as with `Tron msg == Tree (Control.Value -> Maybe msg)`. -}
apply : Tree (Control.Value -> Maybe a) -> Tree (Maybe a)
apply = I.apply



{-| get proxied value from `Tron` -}
getValue : Tree a -> Control.Value
getValue = I.getValue


{-| `zip` maps two GUI trees with given `fn`; if tree structure doesn't match while zipping, the corresponding `Maybe`s
become `Nothing`. NB: When two controls match at the same place in two given trees, the state is taken from the second (right) one. -}
zip : (Maybe a -> Maybe b -> c) -> Tree a -> Tree b -> Tree c
zip = I.zipMap2