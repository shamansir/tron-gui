module Tron.Control.GenUI.Button exposing (to, from, faceTo, faceFrom, toSelectItem)


import GenUI
import Color
import Url

import Tron.Path as Path
import Tron.Control as Core
import Tron.Control.Impl.Button as Button exposing (Control)
import Tron.Style.Theme as Theme

import Tron.Control.GenUI.Color as ColorE

faceTo : Button.Face -> GenUI.Face
faceTo face =
    let
        adaptUrl url =
            {- case url of
                GenUI.Local localUrl -> Just <| Button.toLocalUrl_ localUrl
                GenUI.Remote remoteUrl -> Url.fromString remoteUrl -}
            if Button.isLocal url then
                Button.getLocalPath url |> Maybe.withDefault "" |> GenUI.Local
            else
                GenUI.Remote <| Url.toString url
    in case face of
        Button.Default -> GenUI.Default
        Button.WithIcon (Button.Icon iconFn) ->
            GenUI.OfIcon <|
                case Tuple.mapBoth (Maybe.map adaptUrl) (Maybe.map adaptUrl) ( iconFn Theme.Light, iconFn Theme.Dark ) of
                    ( Just lightUrl, Nothing ) ->
                        [ { theme = GenUI.Light, url = lightUrl } ]
                    ( Just lightUrl, Just darkUrl ) ->
                        [ { theme = GenUI.Light, url = lightUrl }
                        , { theme = GenUI.Dark, url = darkUrl }
                        ]
                    ( Nothing, Just darkUrl ) ->
                        [ { theme = GenUI.Dark, url = darkUrl }
                        ]
                    ( Nothing, Nothing ) ->
                        [ ]
        Button.WithColor color -> GenUI.OfColor <| ColorE.colorToGColor color


faceFrom : GenUI.Face -> Button.Face
faceFrom face =
    let
        findIconByTheme icons theme =
            Nothing -- FIXME
    in case face of
        GenUI.Default -> Button.Default
        GenUI.OfIcon icons -> Button.WithIcon <| Button.Icon <| findIconByTheme icons
        GenUI.OfColor color -> Button.WithColor <| ColorE.gColorToColor color


to : Control a -> GenUI.Def
to (Core.Control face _ _) =
    GenUI.Action
        { face = faceTo face
        }



from : GenUI.Def -> Maybe (Control ())
from def =
    case def of
        GenUI.Action actionDef ->
            Just <|
                Core.Control
                    (faceFrom actionDef.face)
                    ()
                    ()
        _ -> Nothing


toSelectItem : Path.Label -> Control a -> GenUI.SelectItem
toSelectItem value (Core.Control face _ _) =
    { face = faceTo face
    , value = value
    , name = Nothing
    }