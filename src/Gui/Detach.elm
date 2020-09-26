module Gui.Detach exposing (..)


import Url exposing (Url)
import Url.Builder as Url
import Gui.Path as Path exposing (Path, toList)
import Gui.Expose as Exp


type Detach msg =
    Detach
        { toUrl : Path -> Maybe Url
        , send : Exp.PortUpdate -> Cmd msg
        }


map : (msgA -> msgB) -> Detach msgA -> Detach msgB
map f (Detach { toUrl, send }) =
    Detach
        { toUrl = toUrl
        , send = send >> Cmd.map f
        }


never : Detach msg
never =
    Detach { send = always Cmd.none, toUrl = always Nothing }


getUrl : Path -> Detach msg -> Maybe Url
getUrl path (Detach { toUrl }) = toUrl path


formUrl : Url -> Path -> Url
formUrl base path =
    { base
    | fragment =
        path
            |> Path.toList
            |> List.map String.fromInt
            |> String.join "|"
            |> Just
    }


make : (Exp.PortUpdate -> Cmd msg) -> Url -> Detach msg
make send base =
    Detach
        { toUrl = Just << formUrl base
        , send = send
        }


-- extract : Url -> List Path
-- extract
