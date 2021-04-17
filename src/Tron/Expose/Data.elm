module Tron.Expose.Data exposing
    ( Update
    , RawPath, RawClientId
    , Ack, RawInUpdate, RawOutUpdate
    , RawProperty
    )


import Json.Encode as E

import Tron.ProxyValue as ProxyValue exposing (ProxyValue(..))


type alias RawPath =
    List Int


type alias RawProperty =
    E.Value


type alias RawClientId =
    E.Value


type alias Update =
    { path : RawPath
    , value : ProxyValue
    }


type alias RawOutUpdate =
    { path : RawPath
    , value : E.Value
    , stringValue : String
    , labelPath : List String
    , type_ : String
    , client : RawClientId
    }


type alias RawInUpdate =
    { path : RawPath
    , value : E.Value
    , type_ : String
    }


type alias Ack =
    { client : RawClientId
    }
