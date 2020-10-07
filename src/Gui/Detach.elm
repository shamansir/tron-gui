module Gui.Detach exposing (..)


import Url exposing (Url)
import Url.Builder as Url
import Gui.Path as Path exposing (Path, toList)
import Gui.Expose as Exp
import Gui.Property exposing (Property)


type Detach msg =
    Detach
        { toUrl : Path -> Maybe Url
        , sendUpdate : Exp.RawUpdate -> Cmd msg
        , sendTree : Exp.RawProperty -> Cmd msg
        , attached : Maybe Path
        }


map : (msgA -> msgB) -> Detach msgA -> Detach msgB
map f (Detach d) =
    Detach
        { toUrl = d.toUrl
        , sendUpdate = d.sendUpdate >> Cmd.map f
        , sendTree = d.sendTree >> Cmd.map f
        , attached = d.attached
        }


never : Detach msg
never =
    Detach
        { toUrl = always Nothing
        , sendUpdate = always Cmd.none
        , sendTree = always Cmd.none
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


make : (Exp.RawProperty -> Cmd msg) -> (Exp.RawUpdate -> Cmd msg) -> Url -> Detach msg
make sendTree_ sendUpdate_ base =
    Detach
        { toUrl = Just << formUrl base
        , sendUpdate = sendUpdate_
        , sendTree = sendTree_
        , attached = checkAttached base
        }


-- extract : Url -> List Path
-- extract


sendUpdate : Detach msg -> Path -> Property msg -> Cmd msg
sendUpdate (Detach d) path =
    d.sendUpdate << Exp.encodeUpdate path


sendTree : Detach msg -> Property msg -> Cmd msg
sendTree (Detach d) =
    d.sendTree << Exp.encode


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
