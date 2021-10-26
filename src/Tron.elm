module Tron exposing
    ( Tron, Set
    , map, map2, map3, map4, map5
    , mapSet
    , lift
    , andThen, with
    , toUnit, pathify, proxify, expose
    , perform
    )

{-| This is the `Tron a`, which is, similarly to `Html msg` or `Svg msg`, may send your messages into the lifecycle of your application. In this case, it represents your components.

To use Tron in your application, you'll need to specify this function:

    for : Model -> Tron Msg

See `Tron.Builder` for the helpers to define your own interface.

See `WithTron` for the helpers to add `Tron` to your applcation.

# Tron

@docs Tron, Set

# Common helpers

@docs map, mapSet, andThen, with, toUnit
-}

import Task

import Tron.Tree.Internals as Tree exposing (Tree)
import Tron.Tree.Paths as Tree
import Tron.Path as Path exposing (Path)
import Tron.Control.Value exposing (Value)
import Tron.Tree.Expose.Data as Exp


{-| `Tron a` is the tree of your controls or, recursively, any control in such tree.

To build your interface, use the helpers from the `Tron.Builder` module or any of its variants like
`Tron.Builder.Proxy`, `Tron.Builder.Unit` or `Tron.Builder.String`
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
            |> Tron.addLabels Product.getName
        )
        Product.default
        Product.compare
    |> Tron.shape (rows 3)
-}
with : (a -> Tron a -> Tron a) -> Tron a -> Tron a
with f prop = andThen (\a -> f a prop) prop


{-| The usual `map` function which allows you to substitute the messages sent through the components in a `Set`. For example, implementation of `Tron.Builder.palette`:

    Builder.choiceBy
        (options
            |> Builder.buttons
            |> List.map (Tron.with (Builder.face << Builder.useColor << Tuple.second))
            |> addLabels Tuple.first
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


{-| convert usual `Tron a` to `Tron.OfValue a`. Please prefer the one from the `Tron.OfValue` module. -}
lift : Tree a -> Tron a
lift =
    Tree.map (always << Just)


-- proxy : Tron a -> Tron Value
-- proxy = Tree.proxy

map2 : (a -> b -> c) -> Tron a -> Tron b -> Tron c
map2 f =
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


map3 : (a -> b -> c -> d) -> Tron a -> Tron b -> Tron c -> Tron d
map3 f =
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


map4 : (a -> b -> c -> d -> e) -> Tron a -> Tron b -> Tron c -> Tron d -> Tron e
map4 f =
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


map5 : (a -> b -> c -> d -> e -> f) -> Tron a -> Tron b -> Tron c -> Tron d -> Tron e -> Tron f
map5 f =
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


proxify : Tron a -> Tron Value
proxify =
    Tree.map <| always Just


expose : Tron a -> Tron Exp.Value
expose tron =
    Tree.pathify tron
        |> Tree.map (\path val -> Just <| Exp.toRaw path val)