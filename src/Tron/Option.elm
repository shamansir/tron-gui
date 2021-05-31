module Tron.Option exposing
    ( RenderTarget(..), PortCommunication(..) --FIXME: don't expose values
    , noCommunication, sendJson, sendStrings, detachable, withDatGui
    , hidden, toHtml, toDebug, toVr
    )


{-| The configuration of the Tron interface.

# Rendering

@docs RenderTarget, hidden, toHtml, toVr

# Port communication

@docs PortCommunication, noCommunication, sendJson, sendStrings, detachable, withDatGui
-}


import Url exposing (Url)

import Tron.Style.Theme as Theme exposing (Theme(..))
import Tron.Style.Dock as Dock exposing (Dock(..))
import Tron.Expose.Data as Exp
import Tron.Detach as Detach
import Tron.Msg exposing (Msg_)
import Tron.Path exposing (Path)
import Tron.Msg exposing (Msg_(..))


{-| Where to render the GUI:

- Nowhere, the GUI is hidden;
- To HTML, with given Dock and Theme;
- To AFrame (VR), with given Theme (experimental);

-}
type RenderTarget
    = Html Dock Theme
    | Aframe Theme
    | Debug Dock Theme
    | Nowhere


{-| If the GUI communicates with outside world using ports

- It doesn't;
- It sends JSON values to the given ports: see `example/ReportToJsJson` and `Tron.Expose.*` modules' documentation for details;
- It sends stringified to the given ports: see `example/ReportToJsString` and `Tron.Expose.*` modules' documentation for details;
- It is _detachable_, so part of the GUI may be moved to another tab/browser/device and they communicate using WebSocket server using given ports: see `example/Detachable` for details, ensure to run `example/start-server.sh` before running `start-example Detachable`;
- It communicates with `dat.gui` using given ports: see `example/DatGui`for details;
-}
type PortCommunication msg
    = NoCommunication
    | SendJson
        { ack : Exp.RawProperty -> Cmd msg
        , transmit : Exp.RawOutUpdate -> Cmd msg
        }
    | SendStrings
        { transmit : ( List String, String ) -> Cmd msg
        }
    | Detachable
        { ack : Exp.Ack -> Cmd msg
        , transmit : Exp.RawOutUpdate -> Cmd msg
        , receive : Sub Exp.RawInUpdate
        }
    | DatGui
        { ack : Exp.RawProperty -> Cmd msg
        , receive : Sub Exp.RawInUpdate
        }


{- mapPorts : (msgA -> msgB) -> PortCommunication msgA -> PortCommunication msgB
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
                , receive = (d.receive << Sub.map f)
                , toUrl = d.toUrl
                }
        DatGui d ->
            DatGui d -}



{- This is the only function you need to make your `GUI` _detachable*_. However, this function requires some ports to be present as an argument, so you'll need a pair of ports as well. And a WebSocket server. But that's it!

    _*_ — _detachable GUI_ in the context of Web Application means that you may move parts of your user interface to another browser window, tab, or even another device, such as a phone, a tablet, TV, VR glasses or whatever has a browser inside nowadays.

    For a detailed example, see `example/Detachable` in the sources.
    -}


{-| No communication with JS -}
noCommunication : PortCommunication msg
noCommunication = NoCommunication


{-| Send JSON values using given ports:

- `ack` sends the encoded GUI structure at start of the application;
- `transmit` sends the value and path to it in JSON, if it was changed;
 -}
sendJson
    :
        { ack : Exp.RawProperty -> Cmd msg
        , transmit : Exp.RawOutUpdate -> Cmd msg
        }
    -> PortCommunication msg
sendJson = SendJson


{-| Send values as strings using given ports:

- `transmit` sends the value and path to it, when value was changed;
 -}
sendStrings
    :
        { transmit : ( List String, String ) -> Cmd msg
        }
    -> PortCommunication msg
sendStrings = SendStrings


{-| Send information to WebSocket server and receive it from the server.

Only works with `.application` since needs URL access to store Client ID/Path:

    main : ProgramWithTron () Example.Model Example.Msg
    main =
        WithTron.application
            (Option.toHtml Dock.center Theme.light)
            (Option.detachable
                { ack = ack
                , transmit = transmit
                , receive = receieve identity
                }
            )
            { for = ExampleGui.for
            , init = always Example.init
            , view =
                \model ->
                    { title = "Detachable Tron"
                    , body = [ Example.view model ]
                    }
            , update = Example.update
            , subscriptions = always Sub.none
            , onUrlChange = always Example.NoOp
            , onUrlRequest = always Example.NoOp
            }


    port receive : (Exp.RawInUpdate -> msg) -> Sub msg

    port transmit : Exp.RawOutUpdate -> Cmd msg

    port ack : Exp.Ack -> Cmd msg

This needs `example/ws-client.js` and `example/ws-server.js` to exist. The server should be started before running the application with `node ./ws-server.js`, and `ws-client.js` just needs be included in your HTML, and then:

    const app = Elm.YourApp.Main.init({
        node : document.getElementById("elm-node")
    });

    startWs(app.ports.ack, app.ports.receive, app.ports.transmit);


See `example/Detachable` for details.

- `ack` sends the client ID if is new (randomly-generated) or the exising one (was specified in the URL);
- `transmit` sends the JSON value and path to it, when value was changed;
- `receive` receives the value updates from the clients with the same ID;
 -}
detachable
    :
        { ack : Exp.Ack -> Cmd msg
        , transmit : Exp.RawOutUpdate -> Cmd msg
        , receive : Sub Exp.RawInUpdate
        }
    -> PortCommunication msg
detachable = Detachable


{-| Connect with `dat.gui` using given ports:

- `ack` sends the encoded GUI structure at start of the application;
- `receive` receives the value and path to it in JSON, if it was changed in `dat.gui`, and immediately applies it your program;

`dat.gui` library should be included in your HTML file.

See `example/DatGui` for details.
 -}
withDatGui
    :
        { ack : Exp.RawProperty -> Cmd msg
        --, receive : ((Exp.RawInUpdate -> msg) -> Sub msg)
        , receive : Sub Exp.RawInUpdate
        }
    -> PortCommunication msg
withDatGui = DatGui


{-| GUI is hidden. For example, for the case of `dat.gui`, where your interface is on the JS side, but uses Tron definition in Elm.

See `example/DatGui` for details.
-}
hidden : RenderTarget
hidden = Nowhere


{-| Render to HTML using given theme (dark/light) and docked at the requested side (see `Tron.Style.Dock`). Most used option!
-}
toHtml : Dock -> Theme -> RenderTarget
toHtml = Html


{-| Render to Debug mode where all the controls are represented as text boxes with information.
-}
toDebug : Dock -> Theme -> RenderTarget
toDebug = Debug


{-| Render to Virtual Reality using given theme (dark/light); Experimental. Uses `a-frame` library for render, so it should be included in your HTML;

See `example/AFrame` for details.
-}
toVr : Theme -> RenderTarget
toVr = Aframe


