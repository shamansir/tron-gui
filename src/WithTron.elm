module WithTron exposing
    ( Program
    , sandbox, element, document, application
    , overElement, overDocument, overApplication
    , pastDependentOverElement, pastDependentOverDocument, pastDependentOverApplication
    , justUi, justUiAndCommunication
    )


{-| `WithTron` is the set of functions which wrap Elm `Browser.*` helpers and so
let you easily add your GUI to the usual Elm-driven flow.

Here is the `OneKnob` example:

The only things that is different from usual `Elm` application is `for` function which allows you to build the Tron GUI using current state of the model:

    import WithTron exposing (ProgramWithTron)

    type alias Amount = Float

    type Msg
        = AmountChanged Amount

    type alias Model = Amount

    for : Model -> Tron Msg
    for amount =
        Build.root
            [
                ( "amount"
                , Build.float
                    { min = 0, max = 1, step = 0.01 }
                    amount
                    AmountChanged
                )
            ]

    init _ =
        ( 0, Cmd.none )

    view amount =
        Html.text
            <| String.fromFloat amount

    update msg curAmount =
        case msg of

            AmountChanged newAmount ->
                ( newAmount
                , Cmd.none
                )

    subscriptions _ = Sub.none

    main : ProgramWithTron () Model Msg
    main =
        WithTron.element
            (Option.toHtml Dock.center Theme.dark)
            Option.noCommunication
            { for = for
            , init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

*NB*: Please don't forget to add a copy of `src/Tron.css` to your application, or refer to one hosted at GitHub.

For help on how to define your interface with `for` function, see the detailed `Tron.Builder` documentation.

Sometimes  you don't even need the `init`/`view`/`update`, for example to connect GUI to the JavaScript or something remote. In this case the `backed` function lets you define the application which just stores the path-to-value map and nothing else.

More examples are in the `README`, and in the `example/*/Main.elm` modules.

See also: `Tron.Option`, `Tron.Builder`, `Tron.Build.*`.

There are some special types of GUI Builders that can come in handy if you don't want to use messages, but get them as the type-value pairs or add paths to them or just skip them. All of them doesn't require you to specify handling message so, every function from such `Builder` has one argument less:

- `Tron ()` (`Tron.Build.Unit`) — do not expose values (keeps them inside);
- `Tron Value` (`Tron.Build.Proxy`) — store values as a type-value data, see `Tron.Control.Value` for more info;
- `Tron String` (`Tron.Build.String`) — store value stringified;

Any `Tron a` can be converted to `Tron ( Path, Value )` using `Build.addPath`, `Build.addLabeledPath` and/or `Tron.Expose.Convert` helpers.

There is a special `Backed` program, that just stores the stringified values in the `Dict` and is able to send them to the JS, see [Backed](#Backed) section below.

# Program
@docs Program

# Browser.* Wrappers
@docs sandbox, element, document, application

# Helpers to create configurations for `Browser.*`
@docs overElement, overDocument, overApplication

# Past dependent helpers
@docs pastDependentOverElement, pastDependentOverDocument, pastDependentOverApplication

# Just UI
@docs justUi, justUiAndCommunication
-}


import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Url exposing (Url)
import Html exposing (Html)

import Tron.Core exposing (State)
import Tron exposing (Tron)
import Tron as Tron
import Tron.Style.Theme exposing (Theme(..))
import Tron.Style.Dock exposing (Dock(..))
import Tron.Option.Render as Render
import Tron.Option.Communication as Comm
import Tron.Msg exposing (Msg_(..))
import Tron.Tree.Internals as Tree
import Tron.Tree.Controls as Tree
import Tron.Tree.Values as Tree
import Tron.Tree.Paths as Tree

import WithTron.Architecture as Arc


type alias Tree = Tree.Tree ()


{-| Adds `Model msg` to the Elm `Program` and so controls all the required communication between usual App and GUI. -}
type alias Program flags model msg =
    Platform.Program flags ( model, State, Tree ) ( Arc.WithTronMsg msg )


type alias SandboxDef model msg =
    { init : ( model, State, Tree )
    , update : Arc.WithTronMsg msg -> ( model, State, Tree ) -> ( model, State, Tree )
    , view : ( model, State, Tree ) -> Html (Arc.WithTronMsg msg)
    }


type alias ElementDef flags model msg =
    { init : flags -> ( ( model, State, Tree ), Cmd (Arc.WithTronMsg msg) )
    , subscriptions : ( model, State, Tree ) -> Sub (Arc.WithTronMsg msg)
    , update :
          Arc.WithTronMsg msg
          -> ( model, State, Tree )
          -> ( ( model, State, Tree ), Cmd (Arc.WithTronMsg msg) )
    , view : ( model, State, Tree ) -> Html (Arc.WithTronMsg msg)
    }


type alias ApplicationDef flags model msg =
    { init :
          flags
          -> Url
          -> Nav.Key
          -> ( ( model, State, Tree ), Cmd (Arc.WithTronMsg msg) )
    , subscriptions : ( model, State, Tree ) -> Sub (Arc.WithTronMsg msg)
    , update :
          Arc.WithTronMsg msg
          -> ( model, State, Tree )
          -> ( ( model, State, Tree ), Cmd (Arc.WithTronMsg msg) )
    , view :
          ( model, State, Tree )
          -> { body : List (Html (Arc.WithTronMsg msg)), title : String }
    , onUrlChange : Url -> Arc.WithTronMsg msg
    , onUrlRequest : UrlRequest -> Arc.WithTronMsg msg
    }


type alias DocumentDef flags model msg =
    { init : flags -> ( ( model, State, Tree ), Cmd (Arc.WithTronMsg msg) )
    , subscriptions : ( model, State, Tree ) -> Sub (Arc.WithTronMsg msg)
    , update :
          Arc.WithTronMsg msg
          -> ( model, State, Tree )
          -> ( ( model, State, Tree ), Cmd (Arc.WithTronMsg msg) )
    , view :
          ( model, State, Tree )
          -> { body : List (Html (Arc.WithTronMsg msg)), title : String }
    }


{-| Create the configuration that fits `Browser.element`.

    Browser.element
        <| WithTron.overElement
            { ... }
-}
overElement
     : Render.Target
    -> Comm.Ports msg
    ->
        { for : model -> Tron msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> ElementDef flags model msg
overElement renderTarget ports def =
    pastDependentOverElement
        renderTarget
        ports
        { for = always def.for
        , init = def.init
        , subscriptions = always def.subscriptions
        , view = always def.view
        , update = \msg _ model -> def.update msg model
        }


{-| Create the configuration that fits `Browser.element` with the ability to track the previous state of the values in the GUI (see `WithTron.ValueAt`).

    Browser.element
        <| WithTron.pastDependentOverElement
            { ... }
-}
pastDependentOverElement
     : Render.Target
    -> Comm.Ports msg
    ->
        { for : Tree -> model -> Tron msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : Tree -> model -> Sub msg
        , view : Tree -> model -> Html msg
        , update : msg -> Tree -> model -> ( model, Cmd msg )
        }
    -> ElementDef flags model msg
pastDependentOverElement renderTarget ports def =
    { init =
        \flags -> Arc.init ( def.init flags, \tree -> def.for tree >> Tree.toUnit ) Nothing renderTarget ports
    , update =
        Arc.update ( def.update, def.for ) ports
    , view =
        Arc.view def.view renderTarget
    , subscriptions =
        Arc.subscriptions def.subscriptions ports
    }


{-| Create the configuration that fits `Browser.application`.

    Browser.application
        <| WithTron.overApplication
            { ... }
-}
overApplication
     : Render.Target
    -> Comm.Ports msg
    ->
        { init : flags -> Url.Url -> Nav.Key -> ( model, Cmd msg )
        , for : model -> Tron msg
        , subscriptions : model -> Sub msg
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , onUrlChange : Url.Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        }
    -> ApplicationDef flags model msg
overApplication renderTarget ports def =
    pastDependentOverApplication
        renderTarget
        ports
        { for = always def.for
        , init = def.init
        , subscriptions = always def.subscriptions
        , view = always def.view
        , update = \msg _ model -> def.update msg model
        , onUrlChange = def.onUrlChange
        , onUrlRequest = def.onUrlRequest
        }


{-| Create the configuration that fits `Browser.application` with the ability to track the previous state of the values in the GUI (see `WithTron.ValueAt`).

    Browser.element
        <| WithTron.pastDependentOverElement
            { ... }
-}
pastDependentOverApplication
     : Render.Target
    -> Comm.Ports msg
    ->
        { init : flags -> Url.Url -> Nav.Key -> ( model, Cmd msg )
        , for : Tree -> model -> Tron msg
        , subscriptions : Tree -> model -> Sub msg
        , view : Tree -> model -> Browser.Document msg
        , update : msg -> Tree -> model -> ( model, Cmd msg )
        , onUrlChange : Url.Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        }
    -> ApplicationDef flags model msg
pastDependentOverApplication renderTarget ports def =
    { init =
        \flags url key ->
            Arc.init
                ( def.init flags url key, \tree -> def.for tree >> Tree.toUnit ) (Just url) renderTarget ports
    , update =
        Arc.update ( def.update, def.for ) ports
    , view =
        \(userModel, state, tree) ->
            let userView = def.view tree userModel
            in
                { title = userView.title
                , body =
                    [ Arc.view
                        (\_ umodel ->
                            -- FIXME: we calculate view two times, it seems
                            Html.div [] <| userView.body
                        )
                        renderTarget
                        (userModel, state, tree)
                    ]
                }
    , subscriptions =
        Arc.subscriptions def.subscriptions ports
    , onUrlChange =
        \url -> Arc.UrlChange (Just <| def.onUrlChange url) url
    , onUrlRequest =
        \req ->
            case req of
                Internal url ->
                    Arc.UrlChange (Just <| def.onUrlRequest req) <| url
                External _ ->
                    Arc.ToUser <| def.onUrlRequest req
    }


{-| Create the configuration that fits `Browser.document`.

    Browser.document
        <| WithTron.overDocument
            { ... }
-}
overDocument
     : Render.Target
    -> Comm.Ports msg
    ->
        { init : flags -> ( model, Cmd msg )
        , for : model -> Tron msg
        , subscriptions : model -> Sub msg
        , view : model -> Browser.Document msg
        , update : msg -> model -> ( model, Cmd msg )
        }
    -> DocumentDef flags model msg
overDocument renderTarget ports def =
    pastDependentOverDocument
        renderTarget
        ports
        { for = always def.for
        , init = def.init
        , subscriptions = always def.subscriptions
        , view = always def.view
        , update = \msg _ model -> def.update msg model
        }


{-| Create the configuration that fits `Browser.application` with the ability to track the previous state of the values in the GUI (see `WithTron.ValueAt`).

    Browser.document
        <| WithTron.pastDependentOverDocument
            { ... }
-}
pastDependentOverDocument
     : Render.Target
    -> Comm.Ports msg
    ->
        { init : flags -> ( model, Cmd msg )
        , for : Tree -> model -> Tron msg
        , subscriptions : Tree -> model -> Sub msg
        , view : Tree -> model -> Browser.Document msg
        , update : msg -> Tree -> model -> ( model, Cmd msg )
        }
    -> DocumentDef flags model msg
pastDependentOverDocument renderTarget ports def =
    { init =
        \flags ->
            Arc.init ( def.init flags, \tree -> def.for tree >> Tree.toUnit ) Nothing renderTarget ports
    , update =
        Arc.update ( def.update, def.for ) ports
    , view =
        \(userModel, state, tree) ->
            let userView = def.view tree userModel
            in
            { title = userView.title
            , body =
                [ Arc.view
                    (\_ _ ->
                        -- FIXME: we calculate view two times, it seems
                        Html.div [] <| userView.body
                    )
                    renderTarget
                    (userModel, state, tree)
                ]
            }
    , subscriptions =
        Arc.subscriptions def.subscriptions ports
    }


{-| Wrapper for `Program.sandbox` with `for` function and `Tron` options.

For example:

    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Option.Render as Render
    import Tron.Option.Communication as Communication
    import WithTron

    main : WithTron.Program () Example.Model Example.Msg
    main =
        WithTron.sandbox
            (Render.toHtml Dock.center Theme.dark)
            Communication.none
            { for = ExampleGui.for
            , init = Example.init
            , view = Example.view
            , update = Example.update
            }


-}
sandbox
    :  Render.Target
    ->
        { for : model -> Tron msg
        , init : model
        , view : model -> Html msg
        , update : msg -> model -> model
        }
    -> Program () model msg
sandbox renderTarget def =
    Browser.element
        <| overElement renderTarget Comm.none
        <|
        { for = def.for
        , init = always ( def.init, Cmd.none )
        , view = def.view
        , update = \msg model -> ( def.update msg model, Cmd.none )
        , subscriptions = always Sub.none
        }


{-| Wrapper for `Program.element` with `for` function and `Tron` options.

Example from `Basic/Main.elm`

    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Option.Render as Render
    import Tron.Option.Communication as Communication
    import WithTron

    import Example.Goose.Main as Example
    import Example.Goose.Model as Example
    import Example.Goose.Msg as Example
    import Example.Goose.Gui as ExampleGui

    main : WithTron.Program () Example.Model Example.Msg
    main =
        WithTron.element
            (Render.toHtml Dock.center Theme.dark)
            Communication.none
            { for = ExampleGui.for
            , init = always Example.init
            , view = Example.view
            , update = Example.update
            , subscriptions = always Sub.none
            }

 -}
element
    :  Render.Target
    -> Comm.Ports msg
    ->
        { for : Tree -> model -> Tron msg
        , init : flags -> ( model, Cmd msg )
        , subscriptions : Tree -> model -> Sub msg
        , view : Tree -> model -> Html msg
        , update : msg -> Tree -> model -> ( model, Cmd msg )
        }
    -> Program flags model msg
element renderTarget ports def =
    Browser.element
        <| pastDependentOverElement renderTarget ports def


{-| Wrapper for `Program.document` with `for` function and `Tron` options.

For example:

    import WithTron
    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Expose.Data as Exp
    import Tron.Option.Render as Render
    import Tron.Option.Communication as Communication
    import WithTron

    main : WithTron.Program () Example.Model Example.Msg
    main =
        WithTron.document
            (Render.toHtml Dock.center Theme.light)
            (Communication.detachable
                { ack = ackToWs
                , transmit = sendUpdateToWs
                , receive = receieveUpdateFromWs identity
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
            }

 -}
document
    :  Render.Target
    -> Comm.Ports msg
    ->
        { init : flags -> ( model, Cmd msg )
        , for : Tree -> model -> Tron msg
        , subscriptions : Tree -> model -> Sub msg
        , view : Tree -> model -> Browser.Document msg
        , update : msg -> Tree -> model -> ( model, Cmd msg )
        }
    -> Program flags model msg
document renderTarget ports def =
    Browser.document
        <| pastDependentOverDocument renderTarget ports def


{-| Wrapper for `Program.application` with `for` function and `Tron` options.

Example from `Detachable/Main.elm`:

    import WithTron exposing (ProgramWithTron)
    import Tron.Style.Theme as Theme exposing (Theme(..))
    import Tron.Style.Dock as Dock
    import Tron.Expose.Data as Exp
    import Tron.Option.Render as Render
    import Tron.Option.Communication as Communication
    import WithTron

    import Example.Goose.Main as Example
    import Example.Goose.Model as Example
    import Example.Goose.Msg as Example
    import Example.Goose.Gui as ExampleGui

    main : WithTron.Program () Example.Model Example.Msg
    main =
        WithTron.application
            (Render.toHtml Dock.center Theme.light)
            (Communication.detachable
                { ack = ackToWs
                , transmit = sendUpdateToWs
                , receive = receieveUpdateFromWs identity
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

    port receieveUpdateFromWs : (Exp.RawInUpdate -> msg) -> Sub msg

    port sendUpdateToWs : Exp.RawOutUpdate -> Cmd msg

    port ackToWs : Exp.Ack -> Cmd msg

 -}
application
    :  Render.Target
    -> Comm.Ports msg
    ->
        { init : flags -> Url.Url -> Nav.Key -> ( model, Cmd msg )
        , for : Tree -> model -> Tron msg
        , subscriptions : Tree -> model -> Sub msg
        , view : Tree -> model -> Browser.Document msg
        , update : msg -> Tree -> model -> ( model, Cmd msg )
        , onUrlChange : Url.Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        }
    -> Program flags model msg
application renderTarget ports def =
    Browser.application
        <| pastDependentOverApplication renderTarget ports def


{-| -}
justUi
    :  Render.Target
    -> (Tree -> Tree)
    -> ElementDef () () ()
justUi renderTarget for =
    justUiAndCommunication renderTarget Comm.none for


{-| -}
justUiAndCommunication
    :  Render.Target
    -> Comm.Ports ()
    -> (Tree -> Tree)
    -> ElementDef () () ()
justUiAndCommunication renderTarget ports for =
    pastDependentOverElement
        renderTarget
        ports
        <|
            { init = always ( (), Cmd.none )
            , view = \_ _ -> Html.div [] []
            , update = \_ _ _ -> ( (), Cmd.none )
            , subscriptions = \_ _ -> Sub.none
            , for = \tree x -> for tree |> Tron.lift
            }