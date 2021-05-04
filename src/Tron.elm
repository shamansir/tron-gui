module Tron exposing
    ( Tron, Set
    , map, mapSet, andThen, with
    )

{-| This is the `Tron msg`, which is, similarly to `Html msg` or `Svg msg`, may send your messages into the lifecycle of your application. In this case, it represents your components.

To use Tron in your application, you'll need to specify this function:

    for : Model -> Tron Msg

See `Tron.Builder` for the helpers to define your own interface.

See `WithTron` for the helpers to add `Tron` to your applcation.

# Tron

@docs Tron, Set

# Common helpers

@docs map, mapSet, andThen, with
-}

import Tron.Property as Property exposing (Property)


{-| `Tron msg` is the tree of your controls or, recursively, any control in such tree.

To build your interface, use the helpers from the `Tron.Builder` module or any of its variants like
`Tron.Builder.Proxy`, `Tron.Builder.Unit` or `Tron.Builder.String`
-}
type alias Tron msg =
    Property msg


{-| `Set msg` is just the list of controls' definitions together with their labels.
-}
type alias Set msg =
    List ( Property.Label, Tron msg )


{-| The usual `map` function which allows you to substitute the messages sent through the components.
-}
map : (msgA -> msgB) -> Tron msgA -> Tron msgB
map = Property.map


{-| `andThen` without changing a message type
-}
andThen : (msg -> Tron msg) -> Tron msg -> Tron msg
andThen = Property.andThen


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
with : (msg -> Tron msg -> Tron msg) -> Tron msg -> Tron msg
with = Property.with


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
mapSet : (msgA -> msgB) -> Set msgA -> Set msgB
mapSet =
    List.map << Tuple.mapSecond << map

