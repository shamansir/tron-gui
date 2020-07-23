module Elmsfeuer.Gui exposing (..)


foo = 42


{-
view : Model -> Html Msg
view model =
    Html.textarea
        [ Html.style "border" "0px"
        , Html.style "width" "100%"
        , Html.style "height" "100vh"
        , Html.style "overflow" "hidden"
        , Html.style "outline" "none"
        , Html.style "resize" "none"
        ]
        [ Html.text
            <| Encode.encode 4
            <| Gui.encode model
        ]
-}
