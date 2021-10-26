module Tron.Tree.Expose.Json exposing (..)


import Array as Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Color.Convert as Color
import Maybe.Extra as Maybe
import Task


import Tron.Control as Control exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (Form(..))
import Tron.Control.Impl.Text as Text exposing (TextState(..))
import Tron.Control.Impl.Toggle exposing (ToggleState(..))
import Tron.Control.Impl.Button as Button
import Tron.Control.Impl.XY as XY
import Tron.Path as Path exposing (Path)
-- import Tron.Detach as Detach
import Tron.Tree as Tree exposing (..)
import Tron.Tree.Controls as Tree
import Tron.Tree.Paths as Tree
import Tron.Tree.Expose.Data as Exp
import Tron.Control.Value as Value exposing (Value(..))
-- import Tron.Expose.Convert as Exp
import Tron.Style.Theme as Theme
import Tron.Style.PanelShape as PS
import Tron.Style.CellShape as CS


decode : D.Decoder (Tree ())
decode =
    let
        faceAndShape maybeShape maybeFace prop =
            case ( maybeShape, maybeFace ) of
                ( Just ( panelShape, cellShape ), Just face ) ->
                    prop
                        |> Tree.setFace face
                        |> Tree.setPanelShape panelShape
                        |> Tree.setCellShape cellShape
                ( Just ( panelShape, cellShape ), Nothing ) ->
                    prop
                        |> Tree.setPanelShape panelShape
                        |> Tree.setCellShape cellShape
                ( Nothing, Just face ) ->
                    prop
                        |> Tree.setFace face
                ( Nothing, Nothing ) -> prop
    in
    D.field "type" D.string
    |> D.andThen
        (\typeStr ->
            case typeStr of
                "none" -> D.succeed <| Nil ()
                "slider" ->
                    D.map4
                        (\min max step current ->
                            Number
                                <| Control
                                    { min = min
                                    , max = max
                                    , step = step
                                    }
                                    ( Nothing, current )
                                    ()
                        )
                        (D.field "min" D.float)
                        (D.field "max" D.float)
                        (D.field "step" D.float)
                        (D.field "current" D.float)
                "xy" ->
                    D.map7
                        (\minX maxX stepX minY maxY stepY current ->
                            Coordinate
                                <| Control
                                    (
                                        { min = minX
                                        , max = maxX
                                        , step = stepX
                                        }
                                    ,
                                        { min = minY
                                        , max = maxY
                                        , step = stepY
                                        }
                                    )
                                    ( Nothing, current )
                                    ()
                        )
                        (D.field "minX" D.float)
                        (D.field "maxX" D.float)
                        (D.field "stepX" D.float)
                        (D.field "minY" D.float)
                        (D.field "maxY" D.float)
                        (D.field "stepY" D.float)
                        (D.field "current" <|
                            D.map2
                                Tuple.pair
                                (D.field "x" D.float)
                                (D.field "y" D.float)
                        )
                "text" ->
                    D.field "current" D.string
                        |> D.map
                            (\current ->
                                Text <|
                                    Control
                                        ()
                                        ( Ready, current )
                                        ()
                            )
                "color" ->
                    D.field "currentRgba" decodeColor
                        |> D.map
                            (\current ->
                                Color <|
                                    Control
                                        ()
                                        ( Nothing, current )
                                        ()
                            )

                "toggle" ->
                    D.field "current" decodeToggle
                        |> D.map
                            (\current ->
                                Toggle <|
                                    Control
                                        ()
                                        current
                                        ()
                            )

                "button" ->
                    ( D.succeed
                        <| Action
                        <| Control
                            Button.Default
                            ()
                            () )
                    |> D.map2
                        (\maybeFace button ->
                            maybeFace
                                |> Maybe.map (\face -> Tree.setFace face button)
                                |> Maybe.withDefault button
                        )
                        (D.maybe <| D.field "face" decodeFace)

                "nest" ->
                    D.field "nest"
                        (D.array
                            <| D.map2
                                Tuple.pair
                                (D.field "label" D.string)
                                (D.field "property" decode)
                        )
                        |> D.map
                            (\items -> Nest.createGroup items ())
                        |> D.map (Group Nothing Tree.defaultNestShape)
                        |> D.map3 faceAndShape
                            (D.maybe <| D.field "shape" decodeShape)
                            (D.maybe <| D.field "face" decodeFace)

                "choice" ->
                    D.field "options"
                        (D.array
                            <| D.map2
                                Tuple.pair
                                (D.field "label" D.string)
                                (D.field "property" decode)
                        )
                        |> D.map
                            (\items -> Nest.createChoice items ())
                        |> D.map2
                            (\maybeMode control ->
                                case maybeMode of
                                    Just mode -> control |> Nest.setChoiceMode mode
                                    Nothing -> control
                            )
                            (D.maybe <| D.field "mode" decodeChoiceMode)
                        |> D.map (Choice Nothing Tree.defaultNestShape)
                        |> D.map3 faceAndShape
                            (D.maybe <| D.field "shape" decodeShape)
                            (D.maybe <| D.field "face" decodeFace)


                _ -> D.succeed <| Nil () -- or fail?
        )


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

        Number (Control { min, max, step } ( _, val ) _) ->
            E.object
                [ ( "type", E.string "slider" )
                , ( "path", encodePath path )
                , ( "current", E.float val )
                , ( "min", E.float min )
                , ( "max", E.float max )
                , ( "step", E.float step )
                ]

        Coordinate (Control ( xSpec, ySpec ) ( _, ( x, y ) ) _) ->
            E.object
                [ ( "type", E.string "xy" )
                , ( "path", encodePath path )
                , ( "current"
                  , E.object
                        [ ( "x", E.float x )
                        , ( "y", E.float y )
                        ]
                  )
                , ( "minX", E.float xSpec.min )
                , ( "maxX", E.float xSpec.max )
                , ( "stepX", E.float xSpec.step )
                , ( "minY", E.float ySpec.min )
                , ( "maxY", E.float ySpec.max )
                , ( "stepY", E.float ySpec.step )
                ]

        Text (Control _ ( _, val ) _) ->
            E.object
                [ ( "type", E.string "text" )
                , ( "path", encodePath path )
                , ( "current", E.string val )
                ]

        Color (Control _ ( _, val ) _) ->
            E.object
                [ ( "type", E.string "color" )
                , ( "path", encodePath path )
                , ( "current", encodeColor val )
                , ( "currentRgba", encodeRgba val )
                ]

        Toggle (Control _ val _) ->
            E.object
                [ ( "type", E.string "toggle" )
                , ( "path", encodePath path )
                , ( "current"
                  , E.string <|
                        case val of
                            TurnedOn ->
                                "on"

                            TurnedOff ->
                                "off"
                  )
                ]

        Action (Control face _ _) ->
            E.object
                [ ( "type", E.string "button" )
                , ( "path", encodePath path )
                , ( "face", encodeFace face )
                ]

        Choice _ shape (Control items { face, form, selected, mode } _) ->
            E.object
                [ ( "type", E.string "choice" )
                , ( "path", encodePath path )
                , ( "current"
                  , E.int selected
                  )
                , ( "expanded"
                  , E.bool <|
                        case form of
                            Expanded ->
                                True

                            Collapsed ->
                                False

                            Detached ->
                                False
                  )
                , ( "detached"
                  , E.bool <|
                        case form of
                            Detached ->
                                True

                            Collapsed ->
                                False

                            Expanded ->
                                False
                  )
                , ( "options", encodeNested path items )
                , ( "face", face |> Maybe.map encodeFace |> Maybe.withDefault E.null )
                , ( "shape", encodeShape shape )
                , ( "mode", encodeChoiceMode mode )
                ]

        Group _ shape (Control items { face, form } _) ->
            E.object
                [ ( "type", E.string "nest" )
                , ( "path", encodePath path )
                , ( "expanded"
                  , E.bool <|
                        case form of
                            Expanded ->
                                True

                            Collapsed ->
                                False

                            Detached ->
                                False
                  )
                , ( "detached"
                  , E.bool <|
                        case form of
                            Detached ->
                                True

                            Collapsed ->
                                False

                            Expanded ->
                                False
                  )
                , ( "nest", encodeNested path items )
                , ( "face", face |> Maybe.map encodeFace |> Maybe.withDefault E.null )
                , ( "shape", encodeShape shape )
                ]

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


{-
   encodeUpdate : Maybe HashId -> Path -> Tree a -> RawUpdate
   encodeUpdate maybeClient path prop =
       let
           ( type_, value ) =
               case prop of
                   Nil ->
                       ( "none", E.null )
                   Number ( Control _ val _ ) ->
                       ( "slider", E.float val )
                   Coordinate ( Control _ ( x, y ) _ ) ->
                       ( "xy"
                       , E.string <| String.fromFloat x ++ XY.separator ++ String.fromFloat y
                       )
                   Text ( Control _ ( _, val ) _ ) ->
                       ( "text", E.string val )
                   Color ( Control _ val _ ) ->
                       ( "color", encodeColor val )
                   Toggle ( Control _ val _ ) ->
                       ( "toggle"
                       , E.string
                           <| case val of
                               TurnedOn -> "on"
                               TurnedOff -> "off"
                       )
                   Action _ ->
                       ( "button"
                       , E.null
                       )
                   Choice _ _ control ->
                       ( "choice"
                       , E.int <| Nest.whichSelected control
                       )
                   Group _ _ _ ->
                       ( "nest"
                       , E.null
                       )
       in
           { path = Path.toList path
           , value = value
           , type_ = type_
           , client = encodeClientId maybeClient
           }
-}
-- select : Path -> Model msg -> Model msg
-- select selector gui = gui

encodeFace : Button.Face -> E.Value
encodeFace face =
    case face of
        Button.Default -> E.object [ ( "kind", E.string "default" ) ]
        Button.WithIcon (Button.Icon fn) ->
            E.object
                [ ( "kind", E.string "icon" )
                , ( "dark", case fn Theme.Dark of
                        (Button.Url path) -> E.string path
                  )
                , ( "light", case fn Theme.Light of
                        (Button.Url path) -> E.string path
                  )
                ]
        Button.WithColor color ->
            E.object
                [ ( "kind", E.string "color" )
                , ( "color", encodeColor color )
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
                    D.field "color" decodeColor
                        |> D.map Button.WithColor
                "icon" ->
                    D.map2
                        (\darkSrc lightSrc theme ->
                            case theme of
                                Theme.Dark -> Button.Url darkSrc
                                Theme.Light -> Button.Url lightSrc
                        )
                        (D.field "dark" D.string)
                        (D.field "light" D.string)
                        |> D.map (Button.Icon >> Button.WithIcon)
                _ -> D.succeed Button.Default
        )


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



valueDecoder :
    String
    -> D.Decoder Value -- FIXME: move to Value
valueDecoder type_ =
    case type_ of
        "none" ->
            D.succeed None

        "ghost" -> -- backward compatibility
            D.succeed None

        "slider" ->
            D.float |> D.map FromSlider

        "xy" ->
            decodeCoord |> D.map FromXY

        "text" ->
            D.string |> D.map FromInput

        "color" ->
            decodeColor |> D.map FromColor

        "choice" ->
            D.int |> D.map FromChoice

        "toggle" ->
            decodeToggle |> D.map FromToggle

        "button" ->
            D.succeed FromButton

        "nest" ->
            D.succeed FromGroup

        _ ->
            D.succeed None


fromString
    :  String
    -> String
    -> Result String Value -- FIXME: move to Value
fromString type_ str =
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
                |> D.decodeString (D.int |> D.map FromChoice)
                |> Result.mapError D.errorToString

        "toggle" ->
            case str of
                "on" -> Ok <| FromToggle <| TurnedOn
                "off" -> Ok <| FromToggle <| TurnedOff
                _ -> Err str

        "button" ->
            Ok FromButton

        "nest" ->
            Ok FromGroup

        _ ->
            Err str


encodeColor : Color -> E.Value
encodeColor =
    E.string << Color.colorToHexWithAlpha


encodeRgba : Color -> E.Value
encodeRgba color =
    case Color.toRgba color of
        { red, green, blue, alpha } ->
            E.object
                [ ( "red", E.float red )
                , ( "green", E.float green )
                , ( "blue", E.float blue )
                , ( "alpha", E.float alpha )
                ]


decodeColor : D.Decoder Color
decodeColor =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    str
                        |> Color.hexToColor
                        |> Result.map D.succeed
                        |> Result.withDefault (D.fail <| "failed to parse color: " ++ str)
                )
        , D.map4
            Color.rgba
            (D.field "red" D.float)
            (D.field "green" D.float)
            (D.field "blue" D.float)
            (D.field "alpha" D.float)
        ]


decodeCoord : D.Decoder ( Float, Float )
decodeCoord =
    D.oneOf
        [ D.string
            |> D.andThen
            (\str ->
                case str |> String.split XY.separator of
                    v1 :: v2 :: _ ->
                        Maybe.map2
                            Tuple.pair
                            (String.toFloat v1)
                            (String.toFloat v2)
                            |> Maybe.map D.succeed
                            |> Maybe.withDefault (D.fail <| "failed to parse coord: " ++ str)

                    _ ->
                        D.fail <| "failed to parse coord: " ++ str
            )
        , D.map2
            Tuple.pair
            (D.field "x" D.float)
            (D.field "y" D.float)
        ]


decodeToggle : D.Decoder ToggleState
decodeToggle =
    D.oneOf
        [ D.bool
            |> D.map
                (\bool ->
                    if bool then TurnedOn else TurnedOff
                )
        , D.string
            |> D.map (\str ->
                case str of
                    "on" ->
                        TurnedOn

                    "off" ->
                        TurnedOff

                    _ ->
                        TurnedOff
            )
        ]


encodeAck : Exp.Ack -> E.Value
encodeAck { client, tree } =
    E.object
        [ ( "client", client )
        , ( "tree", tree )
        ]