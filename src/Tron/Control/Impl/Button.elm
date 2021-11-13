module Tron.Control.Impl.Button exposing (..)


import Color exposing (Color)
import Tron.Style.Theme exposing (Theme)
import Url exposing (Url)
import Url.Builder as Url

import Tron.Control as Core exposing (Control)
import Tron.Control.Action as A

type Icon = Icon (Theme -> Maybe Url)


type Face
    = Default
    | WithIcon Icon
    | WithColor Color


type alias Control a = Core.Control Face () a


update : A.Action -> Control a -> ( Control a, A.Change )
update action control =
    case action of
        A.Execute ->
            ( control, A.Fire )
        _ ->
            ( control, A.Stay )


{-type alias Transient = ()


getTransientState : Control a -> Transient
getTransientState control = ()


restoreTransientState : Control a -> Transient -> Control a
restoreTransientState control state = control
-}


withIcon : Icon -> Face
withIcon = WithIcon


withColor : Color -> Face
withColor = WithColor


icon : Url -> Icon
icon = Icon << always << Just


themedIcon : (Theme -> Maybe Url) -> Icon
themedIcon = Icon


iconAt : List String -> Icon
iconAt path =
    Icon
        (always
        <| Just
        <| toLocalUrl
        <| path
        )


themedIconAt : (Theme -> List String) -> Icon
themedIconAt f =
    themedIcon
        <| \theme ->
            Just
            <| toLocalUrl
            <| f theme



setFace : Face -> Control a -> Control a
setFace face (Core.Control _ val handler) =
    Core.Control face val handler


-- https://github.com/elm/url/issues/10
-- We would like to use `Url` for icons, but it doesn't support local files,
-- so here we have some dirty hacks;


localMarker : String
localMarker = "@@@@____local___@@@@"


toLocalUrl : List String -> Url
toLocalUrl path =
    toLocalUrl_ <| Url.relative path []


toLocalUrl_ : String -> Url
toLocalUrl_ path =
    { protocol = Url.Https
    , host = localMarker
    , port_ = Nothing
    , path = path
    , query = Nothing
    , fragment = Nothing
    }


encodeMaybeLocalUrl : Url -> String
encodeMaybeLocalUrl url =
    if url.host == localMarker then
        localMarker ++ url.path
    else
        Url.toString url


decodeMaybeLocalUrl : String -> Maybe Url
decodeMaybeLocalUrl str =
    if String.startsWith localMarker str then
        String.dropLeft (String.length localMarker) str
            |> toLocalUrl_
            |> Just
    else Url.fromString str


maybeLocalUrlToString : Url -> String
maybeLocalUrlToString url =
    if url.host == localMarker then
        "./" ++ url.path
    else
        Url.toString url