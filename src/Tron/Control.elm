module Tron.Control exposing (..)


import Task


type Control setup value a =
    Control setup value a -- TODO: `Maybe a`, for `Builder.root`, `Builder.nest` etc.


map : (a -> b) -> Control setup value a -> Control setup value b
map f (Control setup val a) =
    Control setup val <| f a


mapByValue : (value -> b) -> Control setup value a -> Control setup value b
mapByValue f =
    reflect >> map (Tuple.first >> f)


mapWithValue : ((value, a) -> b) -> Control setup value a -> Control setup value b
mapWithValue f =
    reflect >> map f


andThen : (a -> Control setup value b) -> Control setup value a -> Control setup value b
andThen k (Control _ _ a) = k a


apply : Control s v (v -> a) -> Control s v a
apply (Control s v f) =
    Control s v <| f v


with : (Control b c a -> a -> Control b c d) -> Control b c a -> Control b c d
with f control =
    control |> andThen (\a -> f control a)


fold : (a -> x) -> Control s v a -> x
fold handler =
    get >> handler


foldValue : (v -> x) -> Control s v a -> x
foldValue handler =
    getValue >> handler


{-
fold : (msg -> a) -> a -> Control setup value msg -> a
fold f def control=
    case control |> evaluate__ of
        Just msg ->
            f msg
        Nothing ->
            def -}


reflect : Control s v a -> Control s v (v, a)
reflect (Control s v a) =
    Control s v (v, a)


move : Control s v a -> Control s v v
move (Control s v _) =
    Control s v v


update : (v -> v) -> Control s v a -> Control s v a
update f (Control state current a) =
    Control state (f current) a


-- call control with its current value
call : (v -> x) -> Control s v a -> Cmd x
call handler =
    execute__ (Tuple.first >> handler)


-- call control with its current content
execute : (a -> x) -> Control s v a -> Cmd x
execute handler =
    execute__ (Tuple.second >> handler)


-- call control handler with the associated object
execute__ : ((v, a) -> x) -> Control s v a -> Cmd x
execute__ handler (Control _ value a) =
    Task.succeed (value, a)
        |> Task.perform handler


run : Control s v msg -> Cmd msg
run = execute identity


get : Control s v a -> a
get (Control _ _ a) = a


{-| TODO: ensure it is only used internally -}
set : a -> Control s v a -> Control s v a
set a (Control setup value _) = Control setup value a


getValue : Control s v a -> v
getValue (Control _ v _) = v


{-| TODO: ensure it is only used internally -}
setValue : v -> Control s v a -> Control s v a
setValue v = update <| always v


{-
mapWithValue : (v -> msgA -> msgB) -> Control s v msgA -> Control s v msgB
mapWithValue newHandler (Control state current maybeHandler) =
    Control state current
        (maybeHandler |> Maybe.map (\handler v -> newHandler v <| handler v))
-}
