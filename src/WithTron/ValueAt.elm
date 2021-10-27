module WithTron.ValueAt exposing
    ( ask, at
    , Decoder, number, xy, text, toggle, color, action, choice, choiceOf
    , atKnob, atXY, atText, atToggle, atColor, atChoice, atChoiceOf
    , map
    )


{-| `ValueAt` is the way to extract values from the `Tree x` or `Tron x`.

Using `ask` and any `Decoder` you don't have to worry what `Value` is, just do:

    tree |> ask (xy [ "Goose", "Eye" ])

And get `Maybe (Float, Float)` in response. Same works for any of the decoders below:

    tree |> ask (toggle [ "Goose", "Punk" ]) -- returns `Maybe Bool`
    tree |> ask (choice [ "Color Scheme", "Product" ]) -- returns `Maybe (ItemId, Path.Label)`
    tree |> ask (choiceOf Products.all [ "Color Scheme", "Product" ]) -- returns `Maybe Product`
        -- NB: Just ensure to use the very same list you used for creating the `choice` in this case
    tree |> ask (color [ "Feather", "Color" ]) -- returns `Maybe Color`
    -- and so on...

For the example of such, see `example/ForTiler`, where the structure/state of GUI is dependent on current values, but also doesn't store them in its own model, since mostly connects to JavaScript.

# Asking values at path

@docs at

# Actually, all the possible ways to get your value as a common type:

@docs atKnob, atXY, atText, atToggle, atColor, atChoice, atChoiceOf

# Ask using decoders

@docs ask, number, xy, text, toggle, color, action, choice, choiceOf

# Decode

@docs Decoder

# Common helpers

@docs map

-}


import Color exposing (Color)
import Tron.Control.Impl.Nest exposing (ItemId)
import Tron.Control.Value as Proxy
import Tron.Control.Value as Control
import Tron.Path as Path
import Tron.Tree as Tree exposing (Tree)
import Tron.Tree.Paths as Tree


{-| The decoder that knows how to extract a value at the certain path.
-}
type Decoder a
    = Decoder (Control.Value -> Maybe a) (List Path.Label)


{-| Common `map` function for decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder path) =
    Decoder (decoder >> Maybe.map f) path


{-| Load value from the tree using the decoder, and if it's there, you'll get it:

    tree |> ask (xy [ "Goose", "Eye" ]) -- returns `Maybe (Float, Float)`
-}
ask : Decoder a -> Tree x -> Maybe a
ask (Decoder decoder path) =
    Tree.proxify
        >> Tree.findByLabelPath path
        >> Maybe.map Tree.get
        >> Maybe.andThen decoder


make : (Control.Value -> Maybe a) -> List Path.Label -> Decoder a
make =
    Decoder


{-| Load number value by path: works both for `Builr.int` & `Build.float` -}
number : List Path.Label -> Decoder Float
number =
    make Proxy.fromNumber


{-| Load XY value by path -}
xy : List Path.Label -> Decoder ( Float, Float )
xy =
    make Proxy.fromXY


{-| Load text value by path -}
text : List Path.Label -> Decoder String
text =
    make Proxy.fromText


{-| Load Toggle value by path. -}
toggle : List Path.Label -> Decoder Bool
toggle =
    make Proxy.fromToggle


{-| Load chosen item ID (which is `Int`) by path. Use `choiceOf` to get the actual value. -}
choice : List Path.Label -> Decoder ( ItemId, Maybe Path.Label )
choice =
    make Proxy.fromChoice


{-| Load chosen value by path. NB: Ensure to use the very same list you used for creating the `choice` in this case. -}
choiceOf : List a -> List Path.Label -> Decoder a
choiceOf values =
    make <| Proxy.fromChoiceOf values


{-| Has a little sense, but still there. Could be named `absurd`. Check, if button was pressed at least once. -}
action : List Path.Label -> Decoder ()
action =
    make Proxy.fromAction


{-| Load color value by path. -}
color : List Path.Label -> Decoder Color
color =
    make Proxy.fromColor


{-| -}
at : List Path.Label -> Tree a -> Maybe Control.Value
at path =
    Tree.findByLabelPath path
        >> Maybe.map Tree.proxify
        >> Maybe.map Tree.get


{-| -}
atKnob : Float -> List Path.Label -> Tree x -> Float
atKnob default path =
    ask (number path)
        >> Maybe.withDefault default


{-| -}
atXY : ( Float, Float ) -> List Path.Label -> Tree x -> ( Float, Float )
atXY default path =
    ask (xy path)
        >> Maybe.withDefault default


{-| -}
atText : String -> List Path.Label -> Tree x -> String
atText default path =
    ask (text path)
        >> Maybe.withDefault default


{-| -}
atToggle : (Bool -> a) -> a -> List Path.Label -> Tree x -> a
atToggle f default path =
    ask (toggle path)
        >> Maybe.map f
        >> Maybe.withDefault default


{-| -}
atChoice : ( ( ItemId, Maybe Path.Label ) -> a ) -> a -> List Path.Label -> Tree x -> a
atChoice f default path =
    ask (choice path)
        >> Maybe.map f
        >> Maybe.withDefault default


{-| -}
atChoiceOf : List a -> a -> List Path.Label -> Tree x -> a
atChoiceOf values default path =
    ask (choiceOf values path)
        >> Maybe.withDefault default


{-| -}
atColor : Color -> List Path.Label -> Tree x -> Color
atColor default path =
    ask (color path)
        >> Maybe.withDefault default