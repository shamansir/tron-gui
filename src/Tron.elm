module Tron exposing
    ( Tron, Set
    , map
    )

{-| This is the `Tron msg`, which is, similarly to `Html msg` or `Svg msg`, may send your messages into the lifecycle of your application. In this case, it represents your components.

To use Tron in your application, you'll need to specify this function:

    for : Model -> Tron Msg

See `Tron.Builder` for the helpers to define your own interface.

See `WithTron` for the helpers to add `Tron` to your application.

# Tron

@docs Tron, Set

# Common helpers

@docs map
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


{- The usual `map` function which allows you to substitute the messages sent through the components in a `Set`.
-}
{-
mapSet : (msgA -> msgB) -> Set msgA -> Set msgB
mapSet =
    List.map << Tuple.mapSecond << map
-}
