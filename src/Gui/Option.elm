module Gui.Option exposing (..)


import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock as Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Detach as Detach
import Gui.Msg exposing (Msg_)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg_(..))


type RenderTarget
    = Html Dock Theme
    | Aframe Theme
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
        , receive : ((Exp.RawUpdate -> msg) -> Sub msg)
        }
    | DatGui
        { ack : Exp.RawProperty -> Cmd msg
        , receive : ((Exp.RawUpdate -> msg) -> Sub msg)
        }


mapPorts : (msgA -> msgB) -> PortCommunication msgA -> PortCommunication msgB
mapPorts f ports =
    case ports of
        NoCommunication -> NoCommunication
        SendJson { ack, transmit } ->
            SendJson
                { ack = ack >> Cmd.map f
                , transmit = transmit >> Cmd.map f
                }
        SendStrings { transmit } ->
            SendStrings
                { transmit = transmit >> Cmd.map f
                }
        Detachable d ->
            Detachable
                { ack = d.ack >> Cmd.map f
                , transmit = d.transmit >> Cmd.map f
                , receive = d.receive >> Sub.map f
                , toUrl = d.toUrl
                }
        DatGui d ->
            DatGui d



{- This is the only function you need to make your `GUI` _detachable*_. However, this function requires some ports to be present as an argument, so you'll need a pair of ports as well. And a WebSocket server. But that's it!

    _*_ — _detachable GUI_ in the context of Web Application means that you may move parts of your user interface to another browser window, tab, or even another device, such as a phone, a tablet, TV, VR glasses or whatever has a browser inside nowadays.

    For a detailed example, see `example/Detachable` in the sources.
    -}


noCommunication : PortCommunication msg
noCommunication = NoCommunication


sendJson
    :
        { ack : Exp.RawProperty -> Cmd msg
        , transmit : Exp.RawUpdate -> Cmd msg
        }
    -> PortCommunication msg
sendJson = SendJson


sendStrings
    :
        { transmit : ( String, String ) -> Cmd msg
        }
    -> PortCommunication msg
sendStrings = SendStrings


detachable
    :
        { toUrl : Detach.ClientId -> Path -> Maybe Detach.LocalUrl
        , ack : Exp.Ack -> Cmd msg
        , transmit : Exp.RawUpdate -> Cmd msg
        , receive : ((Exp.RawUpdate -> msg) -> Sub msg)
        }
    -> PortCommunication msg
detachable = Detachable


withDatGui
    :
        { ack : Exp.RawProperty -> Cmd msg
        , receive : ((Exp.RawUpdate -> msg) -> Sub msg)
        }
    -> PortCommunication msg
withDatGui = DatGui


hidden : RenderTarget
hidden = Nowhere


toHtml : Dock -> Theme -> RenderTarget
toHtml = Html


toVr : Theme -> RenderTarget
toVr = Aframe


