module Tron.Expose.Data exposing
    ( Property, Update
    , ClientId, Value, Path
    , Ack, In, Out, DeduceIn
    , noValue, noClientId, noInUpdate
    )

{-| The types which are used to communicate b/w Tron and ports (and so JS and WebSockets and `dat.gui`).

# Packages for ports

@docs Ack, In, Out, DeduceIn

# Helpers

@docs noValue, Property, Update, Path, ClientId, Value
-}

import Json.Encode as E

import Tron.Control.Value as Control exposing (Value(..))


type alias Path = List Int


{-| The GUI Tree converted to a JSON value. -}
type alias Property =
    E.Value


{-| Client ID as a JSON value -}
type alias ClientId =
    E.Value


{-| The JSON-friendly representation of any value. -}
type alias Value =
    { path : List Int
    , value : E.Value
    , stringValue : String
    , labelPath : List String
    , type_ : String
    }


{-| Value update with the control `Value`, already converted from JSON. -}
type alias Update =
    { path : List Int
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
    { path : List Int
    , value : E.Value
    , type_ : String
    }


{-| Acknowledge package, which lets the server know the current Client ID. -}
type alias Ack =
    { client : ClientId
    }


{-| The JSON-friendly structure of updates which is received
from the incoming ports by Tron. This one is intended to be deduced,
so the ID-path will be deduced from labelPath,
as well as the `type` of the value will be deduced from
current tree condition. -}
type alias DeduceIn =
    { path : List String
    , value : E.Value
    }


{-| -}
noValue : Value
noValue =
    { path = []
    , value = E.null
    , stringValue = ""
    , labelPath = []
    , type_ = ""
    }


{-| -}
noInUpdate : In
noInUpdate =
    { path = []
    , value = E.null
    , type_ = ""
    }


{-| -}
noClientId : Ack
noClientId = Ack <| E.null