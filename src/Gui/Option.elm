module Gui.Option exposing (..)


import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock as Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Detach as Detach
import Gui.Msg exposing (Msg_)
import Gui.Path exposing (Path)


type RenderTarget
    = Html Dock Theme
    | Aframe
    | Nowhere


type PortCommunication msg
    = NoCommunication
    | SendJson
        { ack : Exp.RawProperty -> Cmd msg
        , transmit : Exp.RawUpdate -> Cmd msg
        }
    | SendStrings
        { transmit : ( String, String ) -> Cmd msg
        }
    | Detachable
        { toUrl : Detach.ClientId -> Path -> Maybe Detach.LocalUrl
        , ack : Exp.Ack -> Cmd msg
        , transmit : Exp.RawUpdate -> Cmd msg
        --, receive : ((Exp.RawUpdate -> msg) -> Sub msg)
        }
    | DatGui {}



type Option msg
    = RenderTo RenderTarget
    | Ports (PortCommunication msg)


map : (a -> b) -> Option a -> Option b
map f opt =
    case opt of
        RenderTo target -> RenderTo target
        Ports NoCommunication -> Ports NoCommunication
        Ports (SendJson { ack, transmit }) ->
            SendJson
                { ack = ack >> Cmd.map f
                , transmit = transmit >> Cmd.map f
                }
                |> Ports
        Ports (SendStrings { transmit }) ->
            SendStrings
                { transmit = transmit >> Cmd.map f
                }
                |> Ports
        Ports (Detachable d) ->
            Detachable
                { ack = d.ack >> Cmd.map f
                , transmit = d.transmit >> Cmd.map f
                --, receive = d.receive >> Sub.map f
                , toUrl = d.toUrl
                }
                |> Ports
        Ports (DatGui d) ->
            Ports <| DatGui d


{- This is the only function you need to make your `GUI` _detachable*_. However, this function requires some ports to be present as an argument, so you'll need a pair of ports as well. And a WebSocket server. But that's it!

    _*_ — _detachable GUI_ in the context of Web Application means that you may move parts of your user interface to another browser window, tab, or even another device, such as a phone, a tablet, TV, VR glasses or whatever has a browser inside nowadays.

    For a detailed example, see `example/Detachable` in the sources.
    -}


sendJson
    :
        { ack : Exp.RawProperty -> Cmd msg
        , transmit : Exp.RawUpdate -> Cmd msg
        }
    -> Option msg
sendJson = Ports << SendJson


sendStrings
    :
        { transmit : ( String, String ) -> Cmd msg
        }
    -> Option msg
sendStrings = Ports << SendStrings


hidden : Option msg
hidden = RenderTo Nowhere


appearance : Dock -> Theme -> Option msg
appearance dock theme = RenderTo <| Html dock theme


aframe : Option msg
aframe = RenderTo Aframe


getRenderTarget : List (Option msg) -> RenderTarget
getRenderTarget =
    List.foldl
        (\option prev ->
            case option of
                RenderTo target -> Just target
                _ -> prev
        )
        Nothing
    >> Maybe.withDefault (Html Dock.center Theme.light)


getCommunication : List (Option msg) -> PortCommunication msg
getCommunication =
    List.foldl
        (\option prev ->
            case option of
                Ports comm -> Just comm
                _ -> prev
        )
        Nothing
    >> Maybe.withDefault NoCommunication
