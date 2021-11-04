module Tron.Tree.Expose.Json exposing
    (decode, encode, fromStrings, valueByTypeDecoder)


import Array as Array exposing (Array)
import Color exposing (Color)
import Json.Decode as D
import Json.Encode as E
import Color.Convert as Color
import Maybe.Extra as Maybe


import Tron.Control exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (Form(..))
import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Toggle as Toggle exposing (ToggleState(..))
import Tron.Control.Impl.Button as Button
import Tron.Control.Json.Text as Text
import Tron.Control.Impl.XY as XY

import Tron.Control.Json.Nest as Nest
-- import Tron.Control.Json.Text exposing (TextState(..))
import Tron.Control.Json.Toggle as Toggle
import Tron.Control.Json.Button as Button
import Tron.Control.Json.Number as Number
import Tron.Control.Json.Color as Color
import Tron.Control.Json.XY as XY

import Tron.Path as Path exposing (Path)
-- import Tron.Detach as Detach
import Tron.Tree.Internals as Tree exposing (..)
import Tron.Tree.Controls as Tree
import Tron.Tree.Paths as Tree
import Tron.Tree.Expose.Data as Exp
import Tron.Control.Value exposing (Value(..))
-- import Tron.Expose.Convert as Exp
import Tron.Style.PanelShape as PS
import Tron.Style.CellShape as CS


decode : D.Decoder (Tree ())
decode =
    D.map2
        Tuple.pair
        (D.field "type" D.string)
        (D.map (Maybe.withDefault False) <| D.maybe <| D.field "live" D.bool)
    |> D.andThen
        (\(typeStr, isLive) ->
            if not isLive then
                decodeByTypeString typeStr
            else
                decodeByTypeString typeStr |> D.map Live
        )


decodeByTypeString : String -> D.Decoder (Tree ())
decodeByTypeString typeStr =
    let
        decodeItem =
            D.map2
                Tuple.pair
                (D.field "label" D.string)
                (D.field "property" decode)
    in
    case typeStr of
        "none" ->
            D.succeed <| Nil ()
        "slider" ->
            Number.decode |> D.map Number
        "xy" ->
            XY.decode |> D.map Coordinate
        "text" ->
            Text.decode |> D.map Text
        "color" ->
            Color.decode |> D.map Color
        "toggle" ->
            Toggle.decode |> D.map Toggle
        "button" ->
            Button.decode |> D.map Action

        "nest" ->
            Nest.decodeGroup
                decodeItem
                |> D.map2
                    (Group Nothing)
                    (D.map (Maybe.withDefault Tree.defaultNestShape)
                        <| D.maybe
                        <| D.field "shape" decodeShape)

        "choice" ->
            Nest.decodeChoice
                decodeItem
                |> D.map2
                    (Choice Nothing)
                    (D.map (Maybe.withDefault Tree.defaultNestShape)
                        <| D.maybe
                        <| D.field "shape" decodeShape)

        _ -> D.succeed <| Nil () -- or fail?



encodeRawPath : List ( Path.Index, Path.Label ) -> E.Value
encodeRawPath =
    E.list <| \(idx, label) -> E.object [ ( "id", E.int idx ), ( "label", E.string label ) ]


encodePath : Path -> E.Value
encodePath =
    Path.toList >> encodeRawPath


encodeTreeAt : Path -> Tree a -> Exp.Tree
encodeTreeAt path property =
    case property of
        Nil _ ->
            E.object
                [ ( "type", E.string "none" )
                , ( "path", encodePath path )
                ]

        Number control ->
            E.object <|
                [ ( "type", E.string "slider" )
                , ( "path", encodePath path )
                ] ++ Number.encode control

        Coordinate control ->
            E.object <|
                [ ( "type", E.string "xy" )
                , ( "path", encodePath path )
                ] ++ XY.encode control

        Text control ->
            E.object <|
                [ ( "type", E.string "text" )
                , ( "path", encodePath path )
                ] ++ Text.encode control

        Color control ->
            E.object <|
                [ ( "type", E.string "color" )
                , ( "path", encodePath path )
                ] ++ Color.encode control

        Toggle control ->
            E.object <|
                [ ( "type", E.string "toggle" )
                , ( "path", encodePath path )
                ] ++ Toggle.encode control

        Action control ->
            E.object <|
                [ ( "type", E.string "button" )
                , ( "path", encodePath path )
                ] ++ Button.encode control

        Choice _ shape control ->
            E.object <|
                [ ( "type", E.string "choice" )
                , ( "path", encodePath path )
                , ( "shape", encodeShape shape )
                ] ++ Nest.encodeChoice (encodeNested path) control

        Group _ shape control ->
            E.object <|
                [ ( "type", E.string "nest" )
                , ( "path", encodePath path )
                , ( "shape", encodeShape shape )
                ] ++ Nest.encodeGroup (encodeNested path) control

        Live innerProp ->
            encodeTreeAt path innerProp


encodeNested : Path -> Array ( Path.Label, Tree a ) -> Exp.Tree
encodeNested path items =
    E.list
        (\( id, ( label, property ) ) ->
            E.object
                [ ( "index", E.int id )
                , ( "label", E.string label )
                , ( "property"
                  , encodeTreeAt
                        (path |> Path.advance ( id, label ))
                        property
                  )
                ]
        )
    <|
        Array.toIndexedList <|
            items


encode : Tree a -> Exp.Tree
encode =
    encodeTreeAt Path.start



encodeShape : Tree.NestShape -> E.Value
encodeShape ( panelShape, cellShape ) =
    E.object
        [
            ( "panel"
            , case PS.numify panelShape of
                ( cols, rows ) ->
                    E.object
                        [ ( "cols", E.int cols )
                        , ( "rows", E.int rows )
                        , ( "pages", E.bool <| PS.pagesEnabled panelShape )
                        ]
            )
        ,
            ( "cell"
            , case CS.units cellShape of
                ( horz, vert ) ->
                    E.object
                        [ ( "horz", encodeCellUnit horz )
                        , ( "vert", encodeCellUnit vert )
                        ]
            )
        ]


decodeShape : D.Decoder NestShape
decodeShape =
    D.map2
        Tuple.pair
        (D.field "panel"
            <| D.map3
                (\cols rows pages ->
                    PS.create ( cols, rows ) |> if pages then PS.manyPages else PS.singlePage
                )
                (D.field "cols" D.int)
                (D.field "rows" D.int)
                (D.map (Maybe.withDefault True)
                    <| D.maybe
                    <| D.field "pages" D.bool
                )
        )
        (D.field "cell"
            <| D.map CS.create
            <| D.map2
                Tuple.pair
                (D.field "horz" decodeCellUnit)
                (D.field "vert" decodeCellUnit)

        )


encodeCellUnit : CS.Unit -> E.Value
encodeCellUnit unit =
    E.string <| case unit of
        CS.Single -> "single"
        CS.Half -> "half"
        CS.Twice -> "twice"


decodeCellUnit : D.Decoder CS.Unit
decodeCellUnit =
    D.string
        |> D.map (\str ->
            case str of
                "single" -> CS.Single
                "half" -> CS.Half
                "twice" -> CS.Twice
                _ -> CS.Single
        )



valueByTypeDecoder :
    String
    -> D.Decoder Value -- FIXME: move to Value
valueByTypeDecoder type_ =
    case type_ of
        "none" ->
            D.succeed None

        "ghost" -> -- backward compatibility
            D.succeed None

        "slider" ->
            D.float |> D.map FromSlider

        "xy" ->
            XY.decodeCoord |> D.map FromXY

        "text" ->
            D.string |> D.map FromInput

        "color" ->
            Color.decodeColor |> D.map FromColor

        "choice" ->
            Nest.decodeSelected |> D.map FromChoice

        "toggle" ->
            Toggle.decodeToggle |> D.map (Toggle.toggleToBool >> FromToggle)

        "button" ->
            D.succeed FromButton

        "nest" ->
            D.succeed FromGroup

        _ ->
            D.succeed None


fromStrings
    :  ( String, String )
    -> Result String Value -- FIXME: move to Value
fromStrings ( type_, str ) =
    case type_ of
        "none" ->
            Ok None

        "ghost" -> -- backward compatible
            Ok None

        "slider" ->
            str
                |> D.decodeString (D.float |> D.map FromSlider)
                |> Result.mapError D.errorToString

        "xy" ->
            case str |> String.split XY.separator of
                v1 :: v2 :: _ ->
                    Maybe.map2
                        Tuple.pair
                        (String.toFloat v1)
                        (String.toFloat v2)
                        |> Maybe.map (FromXY >> Ok)
                        |> Maybe.withDefault (Err <| "failed to parse coord: " ++ str)

                _ ->
                    Err <| "failed to parse coord: " ++ str

        "text" ->
            Ok <| FromInput str

        "color" ->
            Color.hexToColor str |> Result.map FromColor

        "choice" ->
            str
                |> D.decodeString (Nest.decodeSelected |> D.map FromChoice)
                |> Result.mapError D.errorToString

        "toggle" ->
            case str of
                "on" -> Ok <| FromToggle <| Toggle.toggleToBool TurnedOn
                "off" -> Ok <| FromToggle <| Toggle.toggleToBool TurnedOff
                _ -> Err str

        "button" ->
            Ok FromButton

        "nest" ->
            Ok FromGroup

        _ ->
            Err str


-- encodeAck : Exp.Ack -> E.Value
-- encodeAck { client, tree } =
--     E.object
--         [ ( "client", client )
--         , ( "tree", tree )
--         ]