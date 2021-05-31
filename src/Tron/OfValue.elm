module Tron.OfValue exposing
    ( Tron, Set
    , map, mapSet, andThen, with, toUnit, lift
    )

{-| This is the `Tron a`, which is, similarly to `Html msg` or `Svg msg`, may send your messages into the lifecycle of your application. In this case, it represents your components.

To use Tron in your application, you'll need to specify this function:

    for : Model -> Tron Msg

See `Tron.Builder` for the helpers to define your own interface.

See `WithTron` for the helpers to add `Tron` to your applcation.

# Tron

@docs Tron, Set

# Common helpers

@docs map, mapSet, andThen, with, toUnit, lift
-}

import Tron as T
import Tron.Property as Property exposing (Property)
import Tron.Control.Value as Value exposing (Value)


{-| `Tron a` is the tree of your controls or, recursively, any control in such tree.

To build your interface, use the helpers from the `Tron.Builder` module or any of its variants like
`Tron.Builder.Proxy`, `Tron.Builder.Unit` or `Tron.Builder.String`
-}
type alias Tron a =
    T.Tron (Value -> Maybe a)


{-| `Set msg` is just the list of controls' definitions together with their labels.
-}
type alias Set a =
    List ( Property.Label, Tron a )


map : (a -> b) -> Tron a -> Tron b
map f = Property.map <| (<<) (Maybe.map f)


{-| The usual `andThen` function which allows you to change the message type
-}
andThen : (a -> Tron b) -> Tron a -> Tron b
andThen f prop =
    Property.andThen
        (\pF ->
            case pF <| Value.get prop of
                Just v -> f v
                Nothing -> Property.Nil
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
with : (a -> Tron a -> Tron b) -> Tron a -> Tron b
with f prop = andThen (\v -> f v prop) prop


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


{-| -}
lift : T.Tron a -> Tron a
lift = Value.lift