module Tron.Control.Impl.Button exposing (..)


import Color exposing (Color)
import Tron.Style.Theme exposing (Theme)
import Url.Builder as Url

import Tron.Control as Core exposing (Control)
import Tron.Control.Action as A


type Url = Url String


type Icon = Icon (Theme -> Url)


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
            ( control, A.None )


withIcon : Icon -> Face
withIcon = WithIcon


withColor : Color -> Face
withColor = WithColor


icon : Url -> Icon
icon = Icon << always


themedIcon : (Theme -> Url) -> Icon
themedIcon = Icon


iconAt : List String -> Icon
iconAt path = icon <| makeUrl <| Url.relative path []


themedIconAt : (Theme -> List String) -> Icon
themedIconAt f = themedIcon <| \theme -> makeUrl <| Url.relative (f theme) []


makeUrl : String -> Url
makeUrl = Url


urlToString : Url -> String
urlToString (Url str) = str


setFace : Face -> Control a -> Control a
setFace face (Core.Control _ val handler) =
    Core.Control face val handler
