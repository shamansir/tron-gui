module Gui.Option exposing (..)


import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Detach as Detach
import Gui.Msg exposing (Msg_)
import Gui.Path exposing (Path)


type Option msg
    = Hidden
    | Theme Theme
    | Dock Dock
    | SendJsonToJs
        { ack : Exp.RawProperty -> Cmd msg
        , transmit : Exp.RawUpdate -> Cmd msg
        }
    | SendStringsToJs
        { transmit : ( String, String ) -> Cmd msg
        }
    | AFrame
    {- This is the only function you need to make your `GUI` _detachable*_. However, this function requires some ports to be present as an argument, so you'll need a pair of ports as well. And a WebSocket server. But that's it!

    _*_ — _detachable GUI_ in the context of Web Application means that you may move parts of your user interface to another browser window, tab, or even another device, such as a phone, a tablet, TV, VR glasses or whatever has a browser inside nowadays.

    For a detailed example, see `example/Detachable` in the sources.
    -}
    | Detachable
        { toUrl : Detach.ClientId -> Path -> Maybe Detach.LocalUrl
        , ack : Exp.Ack -> Cmd msg
        , send : Exp.RawUpdate -> Cmd msg
        , receive : ((Exp.RawUpdate -> Msg_) -> Sub Msg_)
        }
    | DatGui
        {}
