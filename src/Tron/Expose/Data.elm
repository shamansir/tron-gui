module Tron.Expose.Data exposing
    ( Update
    , RawPath, RawClientId
    , Ack, RawInUpdate, RawOutUpdate
    , RawProperty
    )

{-| The types which are used to communicate b/w Tron and ports (and so JS and WebSockets and `dat.gui`).

# Packages for ports

@docs Ack, RawInUpdate, RawOutUpdate, RawProperty

# Helpers

@docs Update, RawPath, RawClientId
-}

import Json.Encode as E

import Tron.Expose.ProxyValue exposing (ProxyValue(..))


{-| -}
type alias RawPath =
    List Int


{-| The GUI Tree converted to a JSON value. -}
type alias RawProperty =
    E.Value


{-| Client ID as a JSON value -}
type alias RawClientId =
    E.Value


{-| Value update  with the `ProxyValue`. -}
type alias Update =
    { path : RawPath
    , value : ProxyValue
    }


{-| The JSON-friendly structure of updates which is sent
to the outgoing ports from Tron. -}
type alias RawOutUpdate =
    { path : RawPath
    , value : E.Value
    , stringValue : String
    , labelPath : List String
    , type_ : String
    , client : RawClientId
    , chosen : Maybe String
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
