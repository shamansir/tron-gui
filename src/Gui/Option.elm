module Gui.Option exposing (..)


import Gui.Style.Theme as Theme exposing (Theme(..))
import Gui.Style.Dock exposing (Dock(..))
import Gui.Expose as Exp
import Gui.Detach as Detach
import Gui.Msg exposing (Msg_)


type Option msg
    = Hidden
    | Theme Theme
    | Dock Dock
    | SendJsonToJs
        { ack : Exp.RawProperty -> Cmd msg
        , trasmit : Exp.RawUpdate -> Cmd msg
        }
    | SendStringsToJs
        { transmit : ( String, String ) -> Cmd msg
        }
    | AFrame
    | Detachable
        { toUrl : Detach.ClientId -> Path -> Maybe Detach.LocalUrl
        , ack : Exp.Ack -> Cmd msg
        , send : Exp.RawUpdate -> Cmd msg
        , receive : ((Exp.RawUpdate -> Msg_) -> Sub Msg_)
        }
    | DatGui
        {}
