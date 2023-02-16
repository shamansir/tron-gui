module Tron.Control.Json.Nest exposing (..)


import Tron.Path as Path
import Tron.Pages as Pages
import Tron.Control as Core
import Tron.Control.Impl.Nest as Nest exposing (ChoiceControl, GroupControl)
import Tron.Control.Json.Button as Button

import Json.Decode as D
import Json.Encode as E

import Array exposing (Array)


decodeChoice : D.Decoder item -> D.Decoder (ChoiceControl item ())
decodeChoice decodeItem =
    D.map5
        (\items maybeFace maybeMode pageStr maybePageN ->
            Core.Control
                items
                { form = Nest.Collapsed
                , face = maybeFace
                , selected = Pages.firstItem -- FIXME
                , prevSelected = Nothing
                , page = findOutPage ( pageStr, maybePageN )
                , mode = maybeMode |> Maybe.withDefault Nest.Pages
                }
                ()
        )
        (D.field "options" <| D.array decodeItem)
        (D.maybe <| D.field "face" Button.decodeFace)
        (D.maybe <| D.field "mode" decodeChoiceMode)
        (D.field "page" <| D.field "page" D.string)
        (D.field "page" <| D.maybe <| D.field "n" D.int)


encodeChoice : (Array item -> E.Value) -> ChoiceControl item a -> List ( String, E.Value )
encodeChoice encodeNested (Core.Control items { face, form, selected, mode } _) =
    [ ( "current"
        , E.int <| Pages.numifyItem selected
        )
    , ( "expanded"
        , E.bool <|
            case form of
                Nest.Expanded ->
                    True

                Nest.Collapsed ->
                    False

                Nest.Detached ->
                    False
        )
    , ( "detached"
        , E.bool <|
            case form of
                Nest.Detached ->
                    True

                Nest.Collapsed ->
                    False

                Nest.Expanded ->
                    False
        )
    , ( "options", encodeNested items )
    , ( "face", face |> Maybe.map Button.encodeFace |> Maybe.withDefault E.null )
    , ( "mode", encodeChoiceMode mode )
    ]


decodeGroup : D.Decoder item -> D.Decoder (GroupControl item ())
decodeGroup decodeItem =
    D.map4
        (\items maybeFace pageStr maybePageN ->
            Core.Control
                items
                { form = Nest.Collapsed
                , face = maybeFace
                , page = findOutPage ( pageStr, maybePageN )
                }
                ()
        )
        (D.field "nest" <| D.array decodeItem)
        (D.maybe <| D.field "face" Button.decodeFace)
        (D.field "page" <| D.field "page" D.string)
        (D.field "page" <| D.maybe <| D.field "n" D.int)


encodeGroup : (Array item -> E.Value) -> GroupControl item a -> List ( String, E.Value )
encodeGroup encodeNested (Core.Control items { face, form } _) =
    [ ( "expanded"
    , E.bool <|
        case form of
            Nest.Expanded ->
                True

            Nest.Collapsed ->
                False

            Nest.Detached ->
                False
    )
    , ( "detached"
        , E.bool <|
            case form of
                Nest.Detached ->
                    True

                Nest.Collapsed ->
                    False

                Nest.Expanded ->
                    False
        )
    , ( "nest", encodeNested items )
    , ( "face", face |> Maybe.map Button.encodeFace |> Maybe.withDefault E.null )
    ]


encodeChoiceMode : Nest.ChoiceMode -> E.Value
encodeChoiceMode face =
    case face of
        Nest.Pages -> E.object [ ( "kind", E.string "pages" ) ]
        Nest.SwitchThrough -> E.object [ ( "kind", E.string "switch" ) ]
        Nest.Knob -> E.object [ ( "kind", E.string "knob" ) ]


decodeChoiceMode : D.Decoder Nest.ChoiceMode
decodeChoiceMode =
    D.field "kind" D.string
    |> D.andThen
        (\kind ->
            case kind of
                "pages" ->
                    D.succeed Nest.Pages
                "switch" ->
                    D.succeed Nest.SwitchThrough
                "knob" ->
                    D.succeed Nest.Knob
                _ -> D.succeed Nest.Pages
        )


decodeSelected : D.Decoder ( Int, Maybe Path.Label )
decodeSelected =
    D.oneOf
        [ D.string
            |> D.andThen
            (\str ->
                case str |> String.split Nest.separator of
                    v1 :: "" :: _ ->
                        String.toInt v1
                            |> Maybe.map (\n -> (n, Nothing))
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault (D.fail <| "failed to parse choice value: " ++ str)
                    v1 :: v2 :: _ ->
                        String.toInt v1
                            |> Maybe.map (\n -> (n, Just v2))
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault (D.fail <| "failed to parse choice value: " ++ str)
                    v1 :: _ ->
                        String.toInt v1
                            |> Maybe.map (\n -> (n, Nothing))
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault (D.fail <| "failed to parse choice value: " ++ str)
                    _ ->
                        D.fail <| "failed to parse coord: " ++ str
            )
        -- FIXME: use either one in the corresponding place
        , D.map2
            Tuple.pair
            (D.field "id" D.int)
            (D.field "selection" <| D.maybe D.string)
        ]


findOutPage : ( String, Maybe Int ) -> Pages.Page
findOutPage ( pageStr, maybePageN ) =
    case ( pageStr, maybePageN ) of
            ( "n", Just n ) -> Pages.page n
            ( "current", _ ) -> Pages.firstPage  -- FIXME
            ( "first", _ ) -> Pages.firstPage
            ( "last", _ ) -> Pages.firstPage  -- FIXME
            ( _, _ ) -> Pages.firstPage  -- FIXME



{-
        "page":
            { "type": "object"
            , "properties":
                { "page": { "enum": [ "first", "last", "current", "n" ] }
                , "n": { "type": "integer" }
                }
            , "required": [ "page" ]
            },
        "pages":
            { "type": "object"
            , "properties":
                { "distribute": { "enum": [ "auto", "single", "values", "exact" ] }
                , "maxInRow": { "type": "integer" }
                , "maxInColumn": { "type": "integer" }
                , "exact": { "type": "integer" }
                }
            , "required": [ "distribute" ]
            },
-}