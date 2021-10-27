module Tron.Tree.Expose.Data exposing
    ( Tree
    , ClientId, Value
    , Ack, In, Out, DeduceIn
    , nothingGoesIn, noAck, nothingGoesOut
    )

{-| The types which are used to communicate b/w Tron and ports (and so JS and WebSockets and `dat.gui`).

# Packages for ports

@docs Ack, In, Out, DeduceIn, Tree

# Helpers

@docs ClientId, Value

# No-value

@docs nothingGoesIn, noAck, nothingGoesOut
-}

import Json.Encode as E

import Tron.Path as Path



{-| The GUI Tree converted to a JSON value. -}
type alias Tree =
    E.Value


{-| Client ID as a JSON value -}
type alias ClientId =
    E.Value


{-| Acknowledge package, which lets the server know the current Client ID and the initial tree structure. -}
type alias Ack =
    { client : ClientId
    , tree : Tree
    }


{-| The JSON-friendly structure of updates which is received
from the incoming ports by Tron. -}
type alias In =
    { path : List ( Path.Index, Path.Label )
    , value : E.Value
    , type_ : String
    }


{-| The JSON-friendly structure of updates which is sent
to the outgoing ports from Tron. -}
type alias Out =
    { client : ClientId
    , update : Value
    }


{-| The JSON-friendly structure of updates which is received
from the incoming ports by Tron. This one is intended to be deduced,
so that the `type` of the value will be deduced from the
current tree condition. -}
type alias DeduceIn =
    { path : List ( Path.Index, Path.Label )
    , value : E.Value
    }


{-| The JSON-friendly representation of any value. -}
type alias Value =
    { path : List ( Path.Index, Path.Label )
    , value : E.Value
    , stringValue : String
    , type_ : String
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


{-| -}
nothingGoesOut : Out
nothingGoesOut =
    Out E.null noValue


{-| -}
noAck : Ack
noAck = Ack E.null E.null