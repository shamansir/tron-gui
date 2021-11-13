module Tron.Control.Json.Button exposing (..)


import Url
import Tron.Style.Theme as Theme

import Tron.Control as Core
import Tron.Control.Impl.Button as Button exposing (Control)
import Tron.Control.Json.Color as Color

import Json.Decode as D
import Json.Encode as E


decode : D.Decoder (Control ())
decode =
    (D.maybe <| D.field "face" decodeFace)
        |> D.map
            (\maybeFace ->
                Core.Control
                    (maybeFace |> Maybe.withDefault Button.Default)
                    ()
                    ()
            )


encode : Control a -> List ( String, E.Value )
encode (Core.Control face _ _) =
    [ ( "face", encodeFace face )
    ]


encodeFace : Button.Face -> E.Value
encodeFace face =
    case face of
        Button.Default -> E.object [ ( "kind", E.string "default" ) ]
        Button.WithIcon (Button.Icon fn) ->
            E.object
                [ ( "kind", E.string "icon" )
                , ( "dark", case fn Theme.Dark of
                        Just url -> E.string <| Button.encodeMaybeLocalUrl url
                        Nothing -> E.null
                  )
                , ( "light", case fn Theme.Light of
                        Just url -> E.string <| Button.encodeMaybeLocalUrl url
                        Nothing -> E.null
                  )
                ]
        Button.WithColor color ->
            E.object
                [ ( "kind", E.string "color" )
                , ( "color", Color.encodeColor color )
                ]


decodeFace : D.Decoder Button.Face
decodeFace =
    D.field "kind" D.string
    |> D.andThen
        (\kind ->
            case kind of
                "default" ->
                    D.succeed Button.Default
                "color" ->
                    D.field "color" Color.decodeColor
                        |> D.map Button.WithColor
                "icon" ->
                    D.map2
                        (\darkSrc lightSrc theme ->
                            case theme of
                                Theme.Dark -> darkSrc |> Maybe.andThen Button.decodeMaybeLocalUrl
                                Theme.Light -> lightSrc |> Maybe.andThen Button.decodeMaybeLocalUrl
                        )
                        (D.field "dark" <| D.maybe D.string)
                        (D.field "light" <| D.maybe D.string)
                        |> D.map (Button.Icon >> Button.WithIcon)
                _ -> D.succeed Button.Default
        )