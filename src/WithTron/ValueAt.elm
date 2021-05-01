module WithTron.ValueAt exposing
    ( ValueAt
    , Decoder, make, map, ask
    , number, xy, text, toggle, color, action, choice, choiceOf
    )


import Color exposing (Color)

import Tron.Property exposing (LabelPath)
import Tron.Expose.ProxyValue as Proxy exposing (ProxyValue)

import Tron.Control.Nest exposing (ItemId)
import Tron.Control.Toggle exposing (ToggleState)


type alias ValueAt = LabelPath -> Maybe ProxyValue


type Decoder a = Decoder (ValueAt -> Maybe a)


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoder) =
    Decoder (decoder >> Maybe.map f)


ask : Decoder a -> ValueAt -> Maybe a
ask (Decoder decoder) = decoder


make : (ProxyValue -> Maybe a) -> LabelPath -> Decoder a
make convert path =
    Decoder <|
        \valueAt ->
            valueAt path
                |> Maybe.andThen convert


number : LabelPath -> Decoder Float
number = make Proxy.fromNumber


xy : LabelPath -> Decoder ( Float, Float )
xy = make Proxy.fromXY


text : LabelPath -> Decoder String
text = make Proxy.fromText


toggle : LabelPath -> Decoder ToggleState
toggle = make Proxy.fromToggle


choice : LabelPath -> Decoder ItemId
choice =
    make Proxy.fromChoice


choiceOf : List a -> LabelPath -> Decoder a
choiceOf values =
    make <| Proxy.fromChoiceOf values


action : LabelPath -> Decoder ()
action = make Proxy.fromAction


color : LabelPath -> Decoder Color
color = make Proxy.fromColor


-- get : LabelPath -> Decoder a -> ValueAt -> Maybe a

