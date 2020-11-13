module Gui.Render.Plate exposing (..)


import Color
import Url exposing (Url)

import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Detach exposing (ClientId, Detach, getLocalUrl, localUrlToString, LocalUrl)
import Gui.Property exposing (Label, Property)

import Gui.Render.StyleLogic exposing (..)
import Gui.Render.StyleLogic as Style
import Gui.Render.Transform exposing (rotate, scale)
import Gui.Render.Util exposing (arrow)
import Gui.Render.Util as Svg exposing (none)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Attributes as HA
import Html.Events as HE


back : Style.Theme -> Bounds -> Svg Msg
back theme bounds =
    Svg.rect
        [ SA.fill <| Color.toCssString <| Style.background theme
        , SA.x <| String.fromFloat (gap / 2)
        , SA.y <| String.fromFloat (gap / 2)
        , SA.rx <| String.fromFloat borderRadius
        , SA.ry <| String.fromFloat borderRadius
        , SA.width <| String.fromFloat (bounds.width - gap) ++ "px"
        , SA.height <| String.fromFloat (bounds.height - gap) ++ "px"
        ]
        []


controls
    :  Detach msg
    -> Style.Theme
    -> Style.Tone
    -> Path
    -> Bounds
    -> ( Label, Property msg )
    -> Svg Msg
controls detachFn theme tone path bounds ( label, prop ) =
    Svg.g
        [ SA.class <| "plate-controls plate-controls--" ++ toneToModifier tone ]
        [ case detachFn |> getLocalUrl path of
            Just localUrl ->
                detachButton
                    theme
                    tone
                    path
                    localUrl
                    ( gap, gap )
            Nothing -> Svg.none
        , Svg.text_
            [ SA.class "plate-controls__title"
            , SA.x <| String.fromFloat <| bounds.width / 2
            , SA.y <| String.fromFloat <| gap + 1
            , SA.fill <| Color.toCssString <| Style.text theme
            ]
            [ Svg.text label ]
        , collapseButton
            theme
            tone
            path
            ( bounds.width - gap - 10, gap )
        ]


detach : Style.Theme -> Style.Tone -> Svg msg
detach theme tone =
    arrow (Style.colorFor theme tone) (scale 0.35) (rotate 45)


collapse : Style.Theme -> Style.Tone -> Svg msg
collapse theme tone =
    arrow (Style.colorFor theme tone) (scale 0.35) (rotate 180)


collapseButton : Style.Theme -> Style.Tone -> Path -> ( Float, Float ) -> Svg Msg
collapseButton theme tone path (x, y) =
    Svg.g
        [ SA.class "collapse-panel"
        , SA.style <| "transform: translate(" ++
            String.fromFloat x ++ "px,"
            ++ String.fromFloat y ++ "px);"
        , HE.onClick <| Click path
        ]
        [ Svg.rect
            [ SA.fill "transparent"
            , SA.x "-11"
            , SA.y "2.5"
            , SA.width "10"
            , SA.height "10"
            ]
            []
        , collapse theme tone
        ]


detachButton : Style.Theme -> Style.Tone -> Path -> LocalUrl -> ( Float, Float ) -> Svg Msg
detachButton theme tone path localUrl (x, y) =
    Svg.g
        [ SA.class "detach-panel"
        , SA.style <| "transform: translate(" ++
        String.fromFloat x ++ "px," ++ String.fromFloat y ++ "px);"
        , HE.onClick <| Detach path
        ]
        [ Svg.a
            [ SA.xlinkHref <| localUrlToString localUrl
            , SA.target "_blank"
            ]
            [ Svg.rect
                [ SA.fill "transparent"
                , SA.x "0"
                , SA.y "2.5"
                , SA.width "10"
                , SA.height "10"
                ]
                []
            , detach theme tone
            ]
        ]
