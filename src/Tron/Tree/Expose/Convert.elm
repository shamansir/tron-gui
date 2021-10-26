module Tron.Tree.Expose.Convert exposing (..)


import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Json as Exp
import Tron.Tree exposing (Tree(..))
import Tron.Control.Value as Control

import Json.Decode as D
import Json.Encode as E

-- FIXME: move functions below to `Expose.Convert` module? (it is exposed to Public API, so may be not)

swap : Exp.Out -> Exp.In
swap { update } =
    { path = update.path
    , value = update.value
    , type_ = update.type_
    }


loadValue : Exp.Value -> Exp.In
loadValue update =
    { path = update.path
    , value = update.value
    , type_ = update.type_
    }


fromPort : Exp.In -> Exp.Update
fromPort portUpdate =
    { path = portUpdate.path
    , value =
        D.decodeValue
            (Exp.valueDecoder portUpdate.type_)
            portUpdate.value
            |> Result.withDefault Control.None
    }


toProxy : Exp.Out -> Control.Value
toProxy outUpdate =
    fromPort
        (swap outUpdate)
        |> .value


{- encodeAck_ : Detach.ClientId -> Tree a -> E.Value
encodeAck_ clientId tree =
    encodeAck
        { client = Detach.encodeClientId <| Just clientId
        , tree = encode tree
        } -}


makeAck : String -> Tree () -> Exp.Ack
makeAck clientId tree =
    { client = E.string clientId
    , tree = Exp.encode tree
    }


-- TODO: move all below to corresponding controls
