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


type Query = Query String


type alias LocalUrl = ( Query, Fragment )


type alias ClientId = HashId


type State
    = Detached
    | AttachedAt Path


type Detach msg =
    Detach
        { toUrl : ClientId -> Path -> Maybe LocalUrl
        , send : Exp.RawUpdate -> Cmd msg
        , receive : ((Exp.RawUpdate -> Msg) -> Sub Msg)
        , attached : State
        , client : Maybe ClientId
        }


map : (msgA -> msgB) -> Detach msgA -> Detach msgB
map f (Detach d) =
    Detach
        { toUrl = d.toUrl
        , send = d.send >> Cmd.map f
        , receive = d.receive
        , attached = d.attached
        , client = d.client
        }


never : Detach msg
never =
    Detach
        { toUrl = always <| always Nothing
        , send = always Cmd.none
        , receive = always Sub.none
        , attached = attachedAtRoot
        , client = Nothing
        }


nextClientId : Cmd Msg
nextClientId =
    Random.generate SetClientId HashId.generator


setClientId : ClientId -> Detach msg -> Detach msg
setClientId clientId (Detach d) =
    Detach
        { d
        | client = Just clientId
        }


detached : State
detached = Detached


attachedAt : Path -> State
attachedAt = AttachedAt


attachedAtRoot : State
attachedAtRoot = AttachedAt Path.start


getLocalUrl : Path -> Detach msg -> Maybe LocalUrl
getLocalUrl path (Detach d) =
    case d.client of
        Just clientId -> d.toUrl clientId path
        Nothing -> Nothing


root : String
root = "root"


formLocalUrl : ClientId -> Path -> Maybe LocalUrl
formLocalUrl client path =
    Just
        ( Query <| HashId.toString client
        ,
            if Path.howDeep path == 0 then
                Fragment root
            else
                path
                |> Path.toList
                |> List.map String.fromInt
                |> String.join "|"
                |> Fragment
        )


localUrlToString : LocalUrl -> String
localUrlToString ( Query query, Fragment fragment ) =
    "?" ++ query ++ "#" ++ fragment -- TODO: use `elm/url`


make
     : Url
    -> (Exp.RawUpdate -> Cmd msg)
    -> ((Exp.RawUpdate -> Msg) -> Sub Msg)
    -> ( Detach msg, Cmd Msg )
make url sendPort receivePort =
    let
        maybeClient = clientFromUrl url
    in
        ( Detach
            { toUrl = formLocalUrl
            , send = sendPort
            , receive = receivePort
            , attached = fromUrl url
            , client = maybeClient
            }
        , case maybeClient of
            Nothing -> nextClientId
            _ -> Cmd.none
        )


-- extract : Url -> List Path
-- extract


send : Detach msg -> Path -> Property msg -> Cmd msg
send (Detach d) path =
    d.send << Exp.encodeUpdate d.client path


receive : Detach msg -> Sub Msg
receive (Detach d) =
    d.receive ReceiveRaw


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


clientFromUrl : Url -> Maybe ClientId
clientFromUrl { query } =
    query
        |> Maybe.map HashId.fromString


isAttached : Detach msg -> Maybe Path
isAttached (Detach d) =
    d.attached |> stateToMaybe


stateToMaybe : State -> Maybe Path
stateToMaybe state =
    case state of
        Detached -> Nothing
        AttachedAt path -> Just path
