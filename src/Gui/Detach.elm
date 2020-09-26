module Gui.Detach exposing (..)


import Url exposing (Url)
import Url.Builder as Url
import Gui.Path as Path exposing (Path, toList)
import Gui.Expose as Exp
import Gui.Property exposing (Property)


type Detach msg =
    Detach
        { toUrl : Path -> Maybe Url
        , sendUpdate : Exp.PortUpdate -> Cmd msg
        }


map : (msgA -> msgB) -> Detach msgA -> Detach msgB
map f (Detach { toUrl, sendUpdate }) =
    Detach
        { toUrl = toUrl
        , sendUpdate = sendUpdate >> Cmd.map f
        }


never : Detach msg
never =
    Detach { sendUpdate = always Cmd.none, toUrl = always Nothing }


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
make sendUpdate base =
    Detach
        { toUrl = Just << formUrl base
        , sendUpdate = sendUpdate
        }


-- extract : Url -> List Path
-- extract


send : Detach msg -> Path -> Property msg -> Cmd msg
send (Detach { sendUpdate }) path prop =
    sendUpdate (Exp.encodeUpdate path prop)
