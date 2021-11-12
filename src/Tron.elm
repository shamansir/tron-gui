module Tron exposing
    ( Tron, Set
    , map, looseMap2, looseMap3, looseMap4, looseMap5
    , mapSet
    , lift
    , andThen, with
    , toUnit, pathify, proxify, expose
    , perform
    )

{-| This is the `Tron msg`, which is, similarly to `Html msg` or `Svg msg`, may send your messages into the lifecycle of your application.
In this case, it represents the tree of your components with the associated handlers that produce messages.

See [Tutorial](https://github.com/shamansir/tron-gui/blob/main/Tutorial.md) for the details on how to use it.

To use Tron in your application, you'll need to specify this function:

    for : Model -> Tron Msg

See `Tron.Build` for the helpers to define your own interface.

See `WithTron` for the ways to add `Tron` to your applcation.

`Tron msg` is the same as `Tron.Tree (Control.Value -> Maybe msg)`, so it stores the value handler together with every control.

# Tron

@docs Tron, Set

# Convert from and to

@docs lift, toUnit, pathify, proxify, expose

# Common helpers

@docs map, mapSet, andThen, with, looseMap2, looseMap3, looseMap4, looseMap5

# Special

@docs perform
-}

import Task

import Tron.Tree.Internals as Tree exposing (Tree)
import Tron.Tree.Paths as Tree
import Tron.Path as Path exposing (Path)
import Tron.Control.Value exposing (Value)
import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Convert as Exp


{-| `Tron a` is the tree of your controls or, recursively, any control in such tree.

To build your interface, use the helpers from the `Tron.Builder` module or any of its variants like
`Tron.Build.Proxy`, `Tron.Build.Unit` or `Tron.Build.String`
-}
type alias Tron a =
    Tree (Value -> Maybe a)


{-| `Set msg` is just the list of controls' definitions together with their labels.
-}
type alias Set a =
    List ( Path.Label, Tron a )


{-| The usual `map` function which allows you to substitute the messages sent through the components.
-}
map : (a -> b) -> Tron a -> Tron b
map f = Tree.map <| (<<) (Maybe.map f)


{-| The usual `andThen` function which allows you to change the message type
-}
andThen : (a -> Tron b) -> Tron a -> Tron b
andThen f prop =
    Tree.andThen
        (\pF ->
            case pF <| Tree.getValue prop of
                Just v -> f v
                Nothing -> Tree.Nil <| always Nothing
        )
        prop


{-| Same as `andThen`, but also gets current component as argument, it gets useful in mapping `Sets` or lists of controls:

    Tron.choiceBy
        (Product.all
            |> List.filter Product.hasIcon
            |> Tron.buttons
            |> List.map (Tron.with (Tron.face << productIcon))
            |> Tron.toSet Product.getName
        )
        Product.default
        Product.compare
    |> Tron.shape (rows 3)
-}
with : (a -> Tron a -> Tron a) -> Tron a -> Tron a
with f prop = andThen (\a -> f a prop) prop


{-| The usual `map` function which allows you to substitute the messages sent through the components in a `Set`. For example, implementation of `Tron.Build.palette`:

    Tron.choiceBy
        (options
            |> Tron.buttons
            |> List.map (Tron.with (Tron.face << Tron.useColor << Tuple.second))
            |> Tron.toSet Tuple.first
            |> Tron.mapSet Tuple.second
        )
        current
        (\cv1 cv2 ->
            case ( cv1 |> Color.toRgba, cv2 |> Color.toRgba ) of
                ( c1, c2 ) ->
                    (c1.red == c2.red) &&
                    (c1.blue == c2.blue) &&
                    (c1.green == c2.green) &&
                    (c1.alpha == c2.alpha)
        )
        toMsg
    |> cells CS.half
-}
mapSet : (a -> b) -> Set a -> Set b
mapSet =
    List.map << Tuple.mapSecond << map



{-| Store nothing, but values.
-}
toUnit : Tron a -> Tron ()
toUnit =
    map <| always ()


{-| lift `Tree a` to `Tree (Control.Value -> Maybe a)`, which is the alias of `Tron a`.-}
lift : Tree a -> Tron a
lift =
    Tree.map (always << Just)


-- proxy : Tron a -> Tron Value
-- proxy = Tree.proxy

{-| maps two GUI trees with given `fn`; If some control exists only in one tree at the specific compared place, it is removed;
NB: The control state is always taken from the second (right) variant, that's why this `map` is `loose` :). -}
looseMap2 : (a -> b -> c) -> Tron a -> Tron b -> Tron c
looseMap2 f =
    Tree.zipMap2
        (\maybeFToA maybeFToB val ->
            Maybe.map2
                (\fToA fToB -> Maybe.map2 f (fToA val) (fToB val))
                maybeFToA maybeFToB
            |> Maybe.andThen identity
        )
    -- Tree.map2
    --     (\fToA fToB val ->
    --         Maybe.map2 f (fToA val) (fToB val)
    --     )


{-| maps three GUI trees with given `fn`; If one of the trees lacks the control at the specific compared place, this place is empty in the resulting tree;
NB: The control state is always taken from the most right variant, that's why this `map` is `loose` :). -}
looseMap3 : (a -> b -> c -> d) -> Tron a -> Tron b -> Tron c -> Tron d
looseMap3 f =
    Tree.zipMap3
        (\maybeFToA maybeFToB maybeFToC val ->
            Maybe.map3
                (\fToA fToB fToC -> Maybe.map3 f (fToA val) (fToB val) (fToC val))
                maybeFToA maybeFToB maybeFToC
            |> Maybe.andThen identity
        )
    -- Tree.map3
    --     (\fToA fToB fToC val ->
    --         Maybe.map3 f (fToA val) (fToB val) (fToC val)
    --     )


{-| maps four GUI trees with given `fn`; If one of the trees lacks the control at the specific compared place, this place is empty in the resulting tree;
NB: The control state is always taken from the most right variant, that's why this `map` is `loose` :). -}
looseMap4 : (a -> b -> c -> d -> e) -> Tron a -> Tron b -> Tron c -> Tron d -> Tron e
looseMap4 f =
    Tree.zipMap4
        (\maybeFToA maybeFToB maybeFToC maybeFToD val ->
            Maybe.map4
                (\fToA fToB fToC fToD ->
                    Maybe.map4 f (fToA val) (fToB val) (fToC val) (fToD val)
                )
                maybeFToA maybeFToB maybeFToC maybeFToD
            |> Maybe.andThen identity
        )
    -- Tree.map4
    --     (\fToA fToB fToC fToD val ->
    --         Maybe.map4 f (fToA val) (fToB val) (fToC val) (fToD val)
    --     )


{-| maps five GUI trees with given `fn`; If one of the trees lacks the control at the specific compared place, this place is empty in the resulting tree;
NB: The control state is always taken from the most right variant, that's why this `map` is `loose` :). -}
looseMap5 : (a -> b -> c -> d -> e -> f) -> Tron a -> Tron b -> Tron c -> Tron d -> Tron e -> Tron f
looseMap5 f =
    Tree.zipMap5
        (\maybeFToA maybeFToB maybeFToC maybeFToD maybeFToE val ->
            Maybe.map5
                (\fToA fToB fToC fToD fToE ->
                    Maybe.map5 f (fToA val) (fToB val) (fToC val) (fToD val) (fToE val)
                )
                maybeFToA maybeFToB maybeFToC maybeFToD maybeFToE
            |> Maybe.andThen identity
        )
    -- Tree.map5
    --     (\fToA fToB fToC fToD fToE val ->
    --         Maybe.map5 f (fToA val) (fToB val) (fToC val) (fToD val) (fToE val)
    --     )


{-| Apply the value of the given control (at the current level, no going deep) to the handler it holds (`Tron msg`  is `Tree (Control.Value -> Maybe msg)`),
and this way get the current `msg`. Then fire it using `Cmd` as the side-effect, at least if it was determined. -}
perform : Tron msg -> Cmd msg
perform prop =
    Tree.get prop
        |> (\handler ->
                case handler <| Tree.getValue prop of
                    Just msg -> Task.succeed msg |> Task.perform identity
                    Nothing -> Cmd.none
            )


{-| Add the path representing the label-based way to reach the
particular control in the GUI tree.

Tip: to get `Tron (Path, a)`, use:

```
pathify1 : Tron a -> Tron (Path, a)
pathify1 tron =
    Tron.map2
        Tuple.pair
        (Tron.pathify tron)
        tron
```

To get `Tron (Path, Value)`, use:

```
pathify2 : Tron a -> Tron (Path, Value)
pathify2 tron =
    Tron.map2
        Tuple.pair
        (Tron.pathify tron)
        (Tron.proxify tron)
```

-}
pathify : Tron a -> Tron Path
pathify =
    Tree.pathify
        >> Tree.map (always << Just)


{-| Make all the controls in the `Tron` tree return the current value, projected to `Control.Value`, themselves. In combination with `perform` and `map`, this helps to send values or ports of fire them as messages.

Under the hood, since `Tron a == Tree (Control.Value -> Maybe a)`, it becomes `Tree (Control.Value -> Maybe Control.Value)` where it always `Just` with the same value given as argument.
-}
proxify : Tron a -> Tron Value
proxify =
    Tree.map <| always Just


{-| Make all the controls in the `Tron` tree return the current value, converted to the exposed JSON `Expose.Value`, themselves. In combination with `perform`, this helps to send values to ports or fire them as messages.

Under the hood, since `Tron a == Tree (Control.Value -> Maybe a)`, it becomes `Tree (Control.Value -> Maybe Exp.Value)` where it always `Just` with the same value, converted to JSON, given as argument.
-}
expose : Tron a -> Tron Exp.Value
expose tron =
    Tree.pathify tron
        |> Tree.map (\path val -> Just <| Exp.toRaw path val)