module Gui.Detach exposing (..)


import Url exposing (Url)
import Url.Builder as Url
import Gui.Path as Path exposing (Path, toList)


type DetachFn = DetachFn (Path -> Maybe Url)


never : DetachFn
never = DetachFn <| always Nothing


callWith : Path -> DetachFn -> Maybe Url
callWith path (DetachFn fn) = fn path


makeUrlFor : Url -> Path -> Url
makeUrlFor base path =
    { base
    | fragment =
        path
            |> Path.toList
            |> List.map String.fromInt
            |> String.join "|"
            |> Just
    }


default : Url -> DetachFn
default base =
    DetachFn <| (Just << makeUrlFor base)

