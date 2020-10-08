module Gui.Detach exposing (..)


import Url exposing (Url)
import Url.Builder as Url
import Gui.Path as Path exposing (Path, toList)
import Gui.Expose as Exp
import Gui.Property exposing (Property)
import Gui.Msg exposing (Msg(..))


type Detach msg =
    Detach
        { toUrl : Path -> Maybe Url
        , send : Exp.RawUpdate -> Cmd msg
        , receive : ((Exp.RawUpdate -> Msg) -> Sub Msg)
        , attached : Maybe Path
        }


map : (msgA -> msgB) -> Detach msgA -> Detach msgB
map f (Detach d) =
    Detach
        { toUrl = d.toUrl
        , send = d.send >> Cmd.map f
        , receive = d.receive
        , attached = d.attached
        }


never : Detach msg
never =
    Detach
        { toUrl = always Nothing
        , send = always Cmd.none
        , receive = always Sub.none
        , attached = Nothing
        }


getUrl : Path -> Detach msg -> Maybe Url
getUrl path (Detach { toUrl }) = toUrl path


root : String
root = "root"


formUrl : Url -> Path -> Url
formUrl base path =
    { base
    | fragment =
        if Path.howDeep path == 0 then Just "root"
        else path
            |> Path.toList
            |> List.map String.fromInt
            |> String.join "|"
            |> Just
    }


make
     : (Exp.RawUpdate -> Cmd msg)
    -> ((Exp.RawUpdate -> Msg) -> Sub Msg)
    -> Url
    -> Detach msg
make sendPort receivePort base =
    Detach
        { toUrl = Just << formUrl base
        , send = sendPort
        , receive = receivePort
        , attached = checkAttached base
        }


-- extract : Url -> List Path
-- extract


send : Detach msg -> Path -> Property msg -> Cmd msg
send (Detach d) path =
    d.send << Exp.encodeUpdate path


receive : Detach msg -> Sub Msg
receive (Detach d) =
    d.receive ReceiveRaw


-- isAttached : Property msg -> Bool


checkAttached : Url -> Maybe Path
checkAttached url =
    url.fragment
        |> Maybe.map
            (\str ->
                if str == "root"
                then Path.start
                else
                    str
                        |> String.split "|"
                        |> List.map String.toInt
                        |> List.filterMap identity
                        |> Path.fromList
            )


isAttached : Detach msg -> Maybe Path
isAttached (Detach { attached }) = attached
