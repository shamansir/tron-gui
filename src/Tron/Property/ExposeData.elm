module Tron.Property.ExposeData exposing
    ( Property, Update
    , ClientId, Value
    , Ack, In, Out, DeduceIn
    , nothingGoesIn, noAck, nothingGoesOut
    , toRaw
    )

{-| The types which are used to communicate b/w Tron and ports (and so JS and WebSockets and `dat.gui`).

# Packages for ports

@docs Ack, In, Out, DeduceIn

# Helpers

@docs Property, Update, ClientId, Value

# No-value

@docs noValue, noClientId, noInUpdate
-}

import Json.Encode as E

import Tron.Control.Value as Control
import Tron.Control.Value as Value
import Tron.Path as Path exposing (Path, Index, Label)



{-| The GUI Tree converted to a JSON value. -}
type alias Property =
    E.Value


{-| Client ID as a JSON value -}
type alias ClientId =
    E.Value


{-| The JSON-friendly representation of any value. -}
type alias Value =
    { path : List ( Index, Label )
    , value : E.Value
    , stringValue : String
    , type_ : String
    }


{-| Value update with the control `Value`, already converted from JSON. -}
type alias Update =
    { path : List ( Index, Label )
    , value : Control.Value
    }


{-| The JSON-friendly structure of updates which is sent
to the outgoing ports from Tron. -}
type alias Out =
    { client : ClientId
    , update : Value
    }


{-| The JSON-friendly structure of updates which is received
from the incoming ports by Tron. -}
type alias In =
    { path : List ( Index, Label )
    , value : E.Value
    , type_ : String
    }


{-| Acknowledge package, which lets the server know the current Client ID. -}
type alias Ack =
    { client : ClientId
    , tree : Property
    }


{-| The JSON-friendly structure of updates which is received
from the incoming ports by Tron. This one is intended to be deduced,
so the ID-path will be deduced from labelPath,
as well as the `type` of the value will be deduced from
current tree condition. -}
type alias DeduceIn =
    { path : List ( Index, Label )
    , value : E.Value
    }


{-| -}
noValue : Value
noValue =
    { path = []
    , value = E.null
    , stringValue = ""
    , type_ = ""
    }


{-| -}
nothingGoesIn : In
nothingGoesIn =
    { path = []
    , value = E.null
    , type_ = ""
    }


nothingGoesOut : Out
nothingGoesOut =
    Out E.null noValue


{-| -}
noAck : Ack
noAck = Ack E.null E.null


toRaw : Path -> Control.Value -> Value
toRaw path proxyVal =
    { path = Path.toList path
    , type_ = Value.getTypeString proxyVal
    , value = Value.encode proxyVal
    , stringValue = Value.toString proxyVal
    }