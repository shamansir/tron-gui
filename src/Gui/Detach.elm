module Gui.Detach exposing (..)


import Url exposing (Url)
import Url.Builder as Url
import Random
import HashId exposing (HashId)

import Gui.Path as Path exposing (Path, toList)
import Gui.Expose as Exp
import Gui.Property exposing (Property)
import Gui.Msg exposing (Msg(..))


type Fragment = Fragment String


type alias ClientId = HashId


type State
    = Detached
    | AttachedAt Path


type Detach msg =
    Detach
        ( ClientId ->
            { toFragment : Path -> Maybe Fragment
            , send : Exp.RawUpdate -> Cmd msg
            , receive : ((Exp.RawUpdate -> Msg) -> Sub Msg)
            , attached : State
            }
        )


map : (msgA -> msgB) -> Detach msgA -> Detach msgB
map f (Detach dfn) =
    Detach
        (\id ->
            let d = dfn id
            in
                { toFragment = d.toFragment
                , send = d.send >> Cmd.map f
                , receive = d.receive
                , attached = d.attached
                }
        )


never : Detach msg
never =
    Detach
        (\_ ->
            { toFragment = always Nothing
            , send = always Cmd.none
            , receive = always Sub.none
            , attached = attachedAtRoot
            })


nextClientId : Cmd Msg
nextClientId =
    Random.generate SetClientId HashId.generator


detached : State
detached = Detached


attachedAt : Path -> State
attachedAt = AttachedAt


attachedAtRoot : State
attachedAtRoot = AttachedAt Path.start


getFragment : ( ClientId, Path ) -> Detach msg -> Maybe Fragment
getFragment ( id, path ) (Detach f) = (f id |> .toFragment) path


root : String
root = "root"


formFragment : Path -> Maybe Fragment
formFragment path =
    if Path.howDeep path == 0 then Just <| Fragment root
    else path
        |> Path.toList
        |> List.map String.fromInt
        |> String.join "|"
        |> Fragment
        |> Just


fragmentToString : Fragment -> String
fragmentToString (Fragment str) = "#" ++ str


make
     : State
    -> (Exp.RawUpdate -> Cmd msg)
    -> ((Exp.RawUpdate -> Msg) -> Sub Msg)
    -> Detach msg
make state sendPort receivePort =
    Detach
        (\clientId ->
            { toFragment = formFragment
            , send = sendPort
            , receive = receivePort
            , attached = state
            })


-- extract : Url -> List Path
-- extract


send : ( ClientId, Detach msg ) -> Path -> Property msg -> Cmd msg
send ( clientId, Detach f ) path =
    (f clientId |> .send) << Exp.encodeUpdate path


receive : ( ClientId, Detach msg ) -> Sub Msg
receive ( clientId, Detach f ) =
    (f clientId |> .receive) ReceiveRaw


fromUrl : Url -> State
fromUrl { fragment } =
    case fragment of
        Just str ->
            if str == root
            then AttachedAt Path.start
            else
                str
                    |> String.split "|"
                    |> List.map String.toInt
                    |> List.filterMap identity
                    |> Path.fromList
                    |> AttachedAt
        Nothing -> Detached


isAttached : ( ClientId, Detach msg ) -> Maybe Path
isAttached ( clientId, Detach f) =
    f clientId |> .attached |> stateToMaybe


stateToMaybe : State -> Maybe Path
stateToMaybe state =
    case state of
        Detached -> Nothing
        AttachedAt path -> Just path
