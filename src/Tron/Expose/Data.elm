module Tron.Expose.Data exposing
    ( Update
    , RawPath, RawClientId, RawValue
    , Ack, RawInUpdate, RawOutUpdate
    , RawProperty
    , noValue
    )

{-| The types which are used to communicate b/w Tron and ports (and so JS and WebSockets and `dat.gui`).

# Packages for ports

@docs Ack, RawValue, RawInUpdate, RawOutUpdate, RawProperty

# Helpers

@docs Update, RawPath, RawClientId
-}

import Json.Encode as E

import Tron.Control.Value exposing (Value(..))


{-| -}
type alias RawPath =
    List Int


{-| The GUI Tree converted to a JSON value. -}
type alias RawProperty =
    E.Value


{-| Client ID as a JSON value -}
type alias RawClientId =
    E.Value


{-| Value update  with the `Value`. -}
type alias Update =
    { path : RawPath
    , value : Value
    }


{-| The JSON-friendly representation of any value. -}
type alias RawValue =
    { path : RawPath
    , value : E.Value
    , stringValue : String
    , labelPath : List String
    , type_ : String
    }


{-| The JSON-friendly structure of updates which is sent
to the outgoing ports from Tron. -}
type alias RawOutUpdate =
    { client : RawClientId
    , update : RawValue
    }


{-| The JSON-friendly structure of updates which is received
from the incoming ports by Tron. -}
type alias RawInUpdate =
    { path : RawPath
    , value : E.Value
    , type_ : String
    }


{-| Acknowledge package, which lets the server know the current Client ID. -}
type alias Ack =
    { client : RawClientId
    }


noValue : RawValue
noValue =
    { path = []
    , value = E.null
    , stringValue = ""
    , labelPath = []
    , type_ = ""
    }