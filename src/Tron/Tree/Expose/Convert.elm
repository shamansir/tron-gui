module Tron.Tree.Expose.Convert exposing (..)


import Tron.Path as Path exposing (Path)
import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Json as Exp
import Tron.Tree.Internals as Tree exposing (Tree(..), Update)
import Tron.Tree.Paths as Tree
import Tron.Control.Value as Control
import Tron.Control.Value as Value

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


fromPort : Exp.In -> Update
fromPort portUpdate =
    { path = portUpdate.path
    , value =
        D.decodeValue
            (Exp.valueByTypeDecoder portUpdate.type_)
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


toRaw : Path -> Control.Value -> Exp.Value
toRaw path proxyVal =
    { path = Path.toList path
    , type_ = Value.getTypeString proxyVal
    , value = Value.encode proxyVal
    , stringValue = Value.toString proxyVal
    }


{-| Store a `RawOutUpdate` together with message, which is a package that stores all
the required information about the value, such as:

- the path to it in the Tree, both with labels and integer IDs;
- the value in JSON;
- the value as a string;
- the type of the value, as a string;
- client ID, for the communication with WebSockets; (will be removed in future versions)

Use `Builder.map Tuple.first` to get rid of the message if you don't need it.
-}
expose : Tree a -> Tree Exp.Value
expose prop =
    Tree.squeezeMap2
        Tuple.pair
        (Tree.pathify prop)
        (Tree.proxify prop)
    |> Tree.map
        (\( path, proxyVal ) ->
            toRaw path proxyVal
        )
