module WithTron.ValueAt exposing
    ( ValueAt, ask
    , Decoder, number, xy, text, toggle, color, action, choice, choiceOf
    , map
    )

{-| `ValueAt` is the helper to load values from the `Dict`-like storage, which is only used
for `WithTron.Backed` applications to store current values from the UI in universal format. For the cases, when Elm serves only as the UI, and the core of the application logic in in JavaScript.

For the example of such, see `example/ForTiler`, where the structure/state of GUI is dependent on current values, but also doesn't store them in its own model, since mostly connects to JavaScript.

@docs ValueAt, ask


# Decode / Extract

Actually, the way to get your value as a common type.

@docs Decoder, number, xy, text, toggle, color, action, choice, choiceOf


# Common helpers

@docs map

-}

import Color exposing (Color)
import Tron.Control.Nest exposing (ItemId)
import Tron.Control.Toggle exposing (ToggleState)
import Tron.Control.Value as Proxy exposing (Value)
import Tron.Property exposing (LabelPath)


{-| `ValueAt` is the function of type `LabelPath -> Maybe Value`.

Using `ask` and any `Decoder` you don't have to worry what `Value` is, just do:

    valueAt |> ask (xy [ "Goose", "Eye" ])

And get `Maybe (Float, Float)` in response. Same works for any of the decoders below:

    valueAt |> ask (toggle [ "Goose", "Punk" ]) -- returns `Maybe Bool`
    valueAt |> ask (choice [ "Color Scheme", "Product" ]) -- returns `Maybe ItemId`
    valueAt |> ask (choiceOf Products.all [ "Color Scheme", "Product" ]) -- returns `Maybe Product`
        -- NB: Just ensure to use the very same list you used for creating the `choice` in this case
    valueAt |> ask (color [ "Feather", "Color" ]) -- returns `Maybe Color`
    -- and so on...

-}
type alias ValueAt =
    LabelPath -> Maybe Value


{-| The decoder which is able to extract the value.
-}
type Decoder a
    = Decoder (ValueAt -> Maybe a)


{-| Common `map` function for decoder.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder) =
    Decoder (decoder >> Maybe.map f)


{-| Load value from the storage using the decoder, and if it's there, you'll get it:

    valueAt |> ask (xy [ "Goose", "Eye" ]) -- returns `Maybe (Float, Float)`
-}
ask : Decoder a -> ValueAt -> Maybe a
ask (Decoder decoder) =
    decoder


make : (Value -> Maybe a) -> LabelPath -> Decoder a
make convert path =
    Decoder <|
        \valueAt ->
            valueAt path
                |> Maybe.andThen convert


{-| Load number value by path: works both for `Builder.int` & `Builder.float` -}
number : LabelPath -> Decoder Float
number =
    make Proxy.fromNumber


{-| Load XY value by path -}
xy : LabelPath -> Decoder ( Float, Float )
xy =
    make Proxy.fromXY


{-| Load text value by path -}
text : LabelPath -> Decoder String
text =
    make Proxy.fromText


{-| Load Toggle value by path. Use helpers from `Builder` to extract it. -}
toggle : LabelPath -> Decoder ToggleState
toggle =
    make Proxy.fromToggle


{-| Load chosen item ID (which is `Int`) by path. Use `choiceOf` to get the actual value. -}
choice : LabelPath -> Decoder ItemId
choice =
    make Proxy.fromChoice


{-| Load chosen value by path. NB: Ensure to use the very same list you used for creating the `choice` in this case. -}
choiceOf : List a -> LabelPath -> Decoder a
choiceOf values =
    make <| Proxy.fromChoiceOf values


{-| Has a little sense, but still there. Could be named `absurd`. Check, if button was pressed at least once. -}
action : LabelPath -> Decoder ()
action =
    make Proxy.fromAction


{-| Load color value by path. -}
color : LabelPath -> Decoder Color
color =
    make Proxy.fromColor



-- get : LabelPath -> Decoder a -> ValueAt -> Maybe a
