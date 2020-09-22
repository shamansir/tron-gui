module Gui.Control exposing (..)


import Array exposing (Array)

import Task


type Control setup value msg =
    Control setup value (value -> msg)


map : (msgA -> msgB) -> Control setup value msgA -> Control setup value msgB
map f (Control setup val handler) =
    Control setup val (f << handler)


update : (v -> v) -> Control s v msg -> Control s v msg
update f (Control state current handler) =
    Control state (f current) handler


-- call control handler with its current value
call : Control s v msg -> Cmd msg
call (Control _ current handler) =
    callHandler handler current


callHandler : (value -> msg) -> value -> Cmd msg
callHandler handler current =
    Task.succeed current
        |> Task.perform handler


callWith : Control s v msg -> v -> Cmd msg
callWith (Control _ _ handler) val =
    callHandler handler val


-- updateAndExecute : (v -> v) -> Control s v msg -> ( Control s v msg, msg )
