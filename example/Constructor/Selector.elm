module Constructor.Selector exposing (..)


import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html

type Selector a = Selector (List a)


icons =
    [ "animation", "blend", "chromatic", "chtomatics", "cursor"
    , "error", "export", "fog", "link", "loaded", "mp4", "png"
    , "save", "settings", "shuffle", "size", "text", "tick", "tile"
    ]


iconSelector : Selector String
iconSelector = Selector icons


iconSource : String -> List String
iconSource icon = [ "assets", "tiler", "light-stroke", icon ++ ".svg" ]


viewIconSelector : (List String -> msg) -> Html msg
viewIconSelector onSelect =
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
                            ]
                            []
                    )
                <| List.map iconSource
                <| icons_