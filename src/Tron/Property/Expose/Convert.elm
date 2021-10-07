module Tron.Property.Expose.Convert
    exposing
    ( toUnit, toExposed
    , reflect
    )


{-| Make your `Tron *` store the additional information along with messages,
or just get rid of messages at all:

@docs toUnit, toProxied, toExposed, toStrExposed

# Operations on the value

@docs reflect, lift, evaluate

# Internal

@docs toDeferredRaw, encodeAck
-}


import Json.Encode as E
import Dict exposing (Dict)
import HashId exposing (HashId)
import Task

import Tron exposing (Tron)
import Tron.Control as Control
import Tron.Control.Nest as Nest
importTron.Property.Expose.DataExp
import Tron.Expose.Data as Exp
import Tron.Path as Path exposing (Path)
import Tron.Property as Property exposing (..)
import Tron.Control.Value as Value exposing (Value(..))


{-| Replace the `()` value everywhere within Tron GUI tree, it is useful for truly a lot of cases when you don't care about what are the associated values.
-}
toUnit : Tron a -> Tron ()
toUnit = Tron.toUnit


-- FIXME: make it: Tron msg -> Tron RawOutUpdate and use `Property.map2` to join

{-| Store a `RawOutUpdate` together with message, which is a package that stores all
the required information about the value, such as:

- the path to it in the Tree, both with labels and integer IDs;
- the value in JSON;
- the value as a string;
- the type of the value, as a string;
- client ID, for the communication with WebSockets; (will be removed in future versions)

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
toExposed : Tron a -> Tron Exp.Value
toExposed =
    Exp.reflectWithPath
        -- FIXME: `Expose.encodeUpdate` does the same as above
        >> Property.map
            (\( path, proxyVal ) ->
                Just <| proxyToRaw path proxyVal
            )


proxyToRaw : Path -> Value -> Exp.Value
proxyToRaw path proxyVal =
    { path = Path.toList path
    , type_ = Value.getTypeString proxyVal
    , value = Value.encode proxyVal
    , stringValue = Value.toString proxyVal
    }


{-| Extract the value from the control and put it along with a subject of the functor.
-}
reflect : Tron a -> Tron Value
reflect = Exp.reflect >> Property.map Just


-- {-| -}
-- encodeClientId : HashId -> E.Value
-- encodeClientId =
--     HashId.toString >> E.string


-- {-| -}

-- encodeAck : HashId -> Exp.Ack
-- encodeAck =
--     -- encodeClientId
--     Exp.Ack << (HashId.toString >> E.string)