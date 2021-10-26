module Tron.Tree exposing
    ( Tree, FocusAt, Shape, NestShape
    , empty
    , get, getValue
    , map, mapWithPath, mapWithValue
    , andThen, with
    , toUnit, proxify
    , fold, unfold
    , apply
    )


import Tron.Path exposing (Path)
import Tron.Control.Value as Control

import Tron.Tree.Internals as I


type alias Tree a = I.Tree a


type alias FocusAt = I.FocusAt


type alias Shape = I.Shape


type alias NestShape = I.NestShape


empty : Tree ()
empty = I.empty


map : (a -> b) -> Tree a -> Tree b
map = I.map


mapWithPath
    :  (Path -> a -> b)
    -> Tree a
    -> Tree b
mapWithPath =
    I.mapWithPath



mapWithValue
    :  (Path -> Control.Value -> a -> b)
    -> Tree a
    -> Tree b
mapWithValue =
    I.mapWithValue


andThen : (a -> Tree b) -> Tree a -> Tree b
andThen = I.andThen
--andThen f = foldP <| always (get >> f)


with : (a -> Tree a -> Tree b) -> Tree a -> Tree b
-- FIXME: should be changed to `andThen` with getting rid of function in Control
with = I.with


unfold : Tree a -> List (Path, Tree a)
unfold = I.unfold


fold : (Path -> Tree a -> b -> b) -> b -> Tree a -> b
fold = I.fold


get : Tree a -> a
get = I.get

{-| Replace the `()` subject everywhere within Tron GUI tree, it is useful for truly a lot of cases when you don't care about what are the associated values.
-}
toUnit : Tree a -> Tree ()
toUnit = I.toUnit


proxify : Tree a -> Tree Control.Value
proxify = I.proxify


apply : Tree (Control.Value -> Maybe a) -> Tree (Maybe a)
apply = I.apply



{-| get proxied value from `Tron` -}
getValue : Tree a -> Control.Value
getValue = I.getValue