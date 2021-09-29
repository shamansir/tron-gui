module Tron.Option.Communication exposing
    ( Communication, Ports, map
    , noCommunication, sendJson, sendReceiveJson, sendStrings, detachable, withDatGui
    , connect, connect_
    )


{-| The communication of the Tron interface.

@docs Communication, map, noCommunication, sendJson, sendReceiveJson, sendStrings, detachable, withDatGui
-}


import Url exposing (Url)

import Tron.Expose.Data as Exp
import Tron.Detach as Detach
import Tron.Msg exposing (Msg_)
import Tron.Path as Path
import Tron.Msg exposing (Msg_(..))


type alias Ports msg =
    { ack : Maybe (Exp.Ack -> Cmd msg)
    , transmit : Maybe (Exp.Out -> Cmd msg)
    , apply : Maybe (Sub (List Exp.DeduceIn))
    , receive : Maybe (Sub Exp.In)
    }


{-| If the GUI communicates with outside world using ports

- It doesn't;
- It sends JSON values to the given ports: see `example/ReportToJsJson` and `Tron.Expose.*` modules' documentation for details;
- It sends stringified to the given ports: see `example/ReportToJsString` and `Tron.Expose.*` modules' documentation for details;
- It is _detachable_, so part of the GUI may be moved to another tab/browser/device and they communicate using WebSocket server using given ports: see `example/Detachable` for details, ensure to run `example/start-server.sh` before running `start-example Detachable`;
- It communicates with `dat.gui` using given ports: see `example/DatGui`for details;
-}
type Communication msg
    = DontCommunicate
    | Communicate (Ports msg)


mapPorts : (msgA -> msgB) -> Ports msgA -> Ports msgB
mapPorts f p =
    { ack = Maybe.map (\ack -> ack >> Cmd.map f) p.ack
    , transmit = Maybe.map (\transmit -> transmit >> Cmd.map f) p.transmit
    , receive = p.receive
    , apply = p.apply
    }


{-| -}
map : (msgA -> msgB) -> Communication msgA -> Communication msgB
map f communication =
    case communication of
        DontCommunicate -> DontCommunicate
        Communicate ports -> Communicate <| mapPorts f ports


{- This is the only function you need to make your `GUI` _detachable*_. However, this function requires some ports to be present as an argument, so you'll need a pair of ports as well. And a WebSocket server. But that's it!

    _*_ — _detachable GUI_ in the context of Web Application means that you may move parts of your user interface to another browser window, tab, or even another device, such as a phone, a tablet, TV, VR glasses or whatever has a browser inside nowadays.

    For a detailed example, see `example/Detachable` in the sources.
    -}


{-| No communication with JS -}
noCommunication : Communication msg
noCommunication = DontCommunicate


{-| Send JSON values using given ports:

- `ack` sends the encoded GUI structure at start of the application;
- `transmit` sends the value and path to it in JSON, if it was changed;
 -}
sendJson
    :
        { ack : Exp.Property -> Cmd msg
        , transmit : Exp.Out -> Cmd msg
        }
    -> Communication msg
sendJson { ack, transmit } =
    Communicate
        { ack = Just <| .tree >> ack
        , transmit = Just transmit
        , receive = Nothing
        , apply = Nothing
        }


{-| Send JSON values and receive updates using given ports:

- `ack` sends the encoded GUI structure at start of the application;
- `transmit` sends the value and path to it in JSON, if it was changed;
- `apply` gets the list of label paths and values and tries to apply them to
        the current state of the tree, if they match it;
 -}
sendReceiveJson
    :
        { ack : Exp.Property -> Cmd msg
        , transmit : Exp.Out -> Cmd msg
        , apply : Sub (List Exp.DeduceIn)
        }
    -> Communication msg
sendReceiveJson { ack, transmit, apply } =
    Communicate
        { ack = Just <| .tree >> ack
        , transmit = Just transmit
        , receive = Nothing
        , apply = Just apply
        }


{-| Send values as strings using given ports:

- `transmit` sends the value and path to it, when value was changed;
 -}
sendStrings
    :
        { transmit : ( List Path.Label, String ) -> Cmd msg
        }
    -> Communication msg
sendStrings { transmit } =
    Communicate
        { ack = Nothing
        , transmit = Just
            <| \{ update } ->
                transmit ( update.labelPath, update.stringValue )
        , receive = Nothing
        , apply = Nothing
        }


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
        { ack : Exp.ClientId -> Cmd msg
        , transmit : Exp.Out -> Cmd msg
        , receive : Sub Exp.In
        }
    -> Communication msg
detachable { ack, transmit, receive } =
    Communicate
        { ack = Just <| .client >> ack
        , transmit = Just transmit
        , receive = Just receive
        , apply = Nothing
        }


{-| Connect with `dat.gui` using given ports:

- `ack` sends the encoded GUI structure at start of the application;
- `receive` receives the value and path to it in JSON, if it was changed in `dat.gui`, and immediately applies it your program;

`dat.gui` library should be included in your HTML file.

See `example/DatGui` for details.
 -}
withDatGui
    :
        { ack : Exp.Property -> Cmd msg
        --, receive : ((Exp.RawInUpdate -> msg) -> Sub msg)
        , receive : Sub Exp.In
        }
    -> Communication msg
withDatGui { ack, receive } =
    Communicate
        { ack = Just <| .tree >> ack
        , transmit = Nothing
        , receive = Just receive
        , apply = Nothing
        }


{-| Use all the communication you can imagine. -}
connect :
    { ack : Exp.Ack -> Cmd msg
    , transmit : Exp.Out -> Cmd msg
    , apply : Sub (List Exp.DeduceIn)
    , receive : Sub Exp.In
    } -> Communication msg
connect spec =
    Communicate
        { ack = Just spec.ack
        , transmit = Just spec.transmit
        , apply = Just spec.apply
        , receive = Just spec.receive
        }



{-| Use some of the communication you can imagine. -}
connect_ : Ports msg -> Communication msg
connect_ = Communicate
