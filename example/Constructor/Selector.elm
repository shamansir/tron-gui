module Constructor.Selector exposing (..)


import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html

import Tron.Control.Button as Button

type Selector a = Selector (List a)


icons =
    [ "animation", "blend", "chromatic", "chtomatics", "cursor"
    , "error", "export", "fog", "link", "loaded", "mp4", "png"
    , "save", "settings", "shuffle", "size", "text", "tick", "tile"
    ]


iconSelector : Selector String
iconSelector = Selector icons


iconSource : String -> List String
iconSource icon =
    [ "assets", "tiler", "light-stroke", icon ++ ".svg" ]


viewIconSelector : Maybe Button.Url -> (List String -> msg) -> Html msg
viewIconSelector maybeCurrent onSelect =
    case iconSelector of
        Selector icons_ ->
            Html.div
                [ Html.class "icons"
                ]
                <| List.map
                    (\iconSrc ->
                        Html.img
                            [ Html.src <| String.join "/" iconSrc
                            , Html.onClick <| onSelect iconSrc
                            , Html.class <| case maybeCurrent of
                                Just (Button.Url currentUrl) ->
                                    if String.join "/" iconSrc == currentUrl then "current" else ""
                                Nothing -> ""
                            ]
                            []
                    )
                <| List.map iconSource
                <| icons_