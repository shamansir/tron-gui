module Tron.Control exposing (..)


import Array exposing (Array)

import Task


type Control setup value msg =
    Control setup value (Maybe (value -> msg))


map : (msgA -> msgB) -> Control setup value msgA -> Control setup value msgB
map f (Control setup val handler) =
    Control setup val
        (handler |> Maybe.map ((<<) f))


update : (v -> v) -> Control s v msg -> Control s v msg
update f (Control state current handler) =
    Control state (f current) handler


-- call control handler with its current value
call : Control s v msg -> Cmd msg
call (Control _ current handler) =
    handler
        |> Maybe.map (callHandler current)
        |> Maybe.withDefault Cmd.none


callHandler : value -> (value -> msg) -> Cmd msg
callHandler value handler =
    Task.succeed value
        |> Task.perform handler


callWith : Control s v msg -> v -> Cmd msg
callWith (Control _ _ handler) val =
    handler
        |> Maybe.map (callHandler val)
        |> Maybe.withDefault Cmd.none


-- FIXME: should be removed with changing on storing message, not a function
-- also calling it w/o `Cmd` is bad
evaluate__ : Control s v msg -> Maybe msg
evaluate__ (Control _ current handler) =
    handler
        |> Maybe.map (\f -> f current)


getValue : Control s v msg -> v
getValue (Control _ v _) = v


setValue : v -> Control s v msg -> Control s v msg
setValue v = update <| always v


setHandler : (v -> msgB) -> Control s v msgA -> Control s v msgB
setHandler newHandler (Control state current _) =
    Control state current <| Just newHandler


mapWithValue : (v -> msgA -> msgB) -> Control s v msgA -> Control s v msgB
mapWithValue newHandler (Control state current maybeHandler) =
    Control state current
        (maybeHandler |> Maybe.map (\handler v -> newHandler v <| handler v))
