module HashId exposing (..)


import Random
import Hashids


type HashId = HashId String


seed : String
seed = "Tron UI v2.0"


context : Hashids.Context
context = Hashids.hashidsMinimum seed 8


generator : Random.Generator HashId
generator =
    Random.list 8 (Random.int 0 255)
        |> Random.map (Hashids.encodeList context >> HashId)
