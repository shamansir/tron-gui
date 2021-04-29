module Tron.Expose exposing (..)

-- TODO: make controls expose themselves, so get rid of these imports below

import Array as Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import HashId exposing (HashId)
import Json.Decode as D
import Json.Encode as E
import Color.Convert as Color


import Tron.Control as Control exposing (..)
import Tron.Control.Nest as Nest exposing (Form(..))
import Tron.Control.Text as Text exposing (TextState(..))
import Tron.Control.Toggle exposing (ToggleState(..))
import Tron.Control.XY as XY
import Tron.Path as Path exposing (Path)
import Tron.Property exposing (..)
import Tron.Expose.ProxyValue as ProxyValue exposing (ProxyValue(..))
import Tron.Expose.Data exposing (..)
import Tron.Expose.Convert exposing (..)


updateProperty : ProxyValue -> Property msg -> Cmd msg
updateProperty value property =
    case ( property, value ) of
        ( Nil, _ ) ->
            Cmd.none

        ( Number control, FromSlider f ) ->
            f |> callWith control

        ( Coordinate control, FromXY xy ) ->
            xy |> callWith control

        ( Text control, FromInput s ) ->
            ( Ready, s ) |> callWith control

        ( Color control, FromColor c ) ->
            c |> callWith control

        ( Toggle control, FromToggle t ) ->
            t |> callWith control

        ( Action control, FromButton ) ->
            () |> callWith control

        ( Choice _ _ control, FromChoice i ) ->
            let
                curValue =
                    Control.getValue control
            in
            { curValue
                | selected = i
            }
                |> callWith control

        ( Group _ _ _, _ ) ->
            Cmd.none

        ( _, _ ) ->
            Cmd.none


update : Update -> Property msg -> Cmd msg
update { path, value } prop =
    case path of
        [] ->
            updateProperty value prop

        id :: next ->
            case prop of
                Group _ _ control ->
                    case control |> Nest.get id of
                        Just ( _, innerProp ) ->
                            innerProp
                                |> update { path = next, value = value }

                        Nothing ->
                            Cmd.none

                _ ->
                    Cmd.none


applyProperty : ProxyValue -> Property msg -> Property msg
applyProperty value prop =
    case ( prop, value ) of
        ( Nil, _ ) ->
            prop

        ( Number control, FromSlider f ) ->
            control |> Control.setValue f |> Number

        ( Coordinate control, FromXY xy ) ->
            control |> Control.setValue xy |> Coordinate

        ( Text control, FromInput s ) ->
            control |> Control.setValue ( Ready, s ) |> Text

        ( Color control, FromColor c ) ->
            control |> Control.setValue c |> Color

        ( Toggle control, FromToggle t ) ->
            control |> Control.setValue t |> Toggle

        ( Action control, FromButton ) ->
            control |> Control.setValue () |> Action

        ( Choice focus shape control, FromChoice i ) ->
            Choice focus shape <| Nest.select i <| control

        ( Group _ _ _, _ ) ->
            prop

        ( _, _ ) ->
            prop


apply : Update -> Property msg -> Property msg
apply { path, value } prop =
    case path of
        [] ->
            applyProperty value prop

        id :: next ->
            case prop of
                Group focus shape control ->
                    control
                        |> Nest.withItem id
                            (Tuple.mapSecond <| apply { path = next, value = value })
                        |> Group focus shape

                Choice focus shape control ->
                    control
                        |> Nest.withItem id
                            (Tuple.mapSecond <| apply { path = next, value = value })
                        |> Choice focus shape

                _ ->
                    prop


loadValues : Dict LabelPath String -> Property msg -> Property msg
loadValues dict prop =
    Tron.Property.replaceWithLabeledPath
        (\labelPath innerProp ->
            Dict.get labelPath dict
                |> Maybe.andThen (\strValue -> applyStringValue strValue innerProp)
                |> Maybe.withDefault innerProp
        )
        prop


loadProxyValues : Dict (List Int) ProxyValue -> Property msg -> Property msg
loadProxyValues dict prop =
    dict
        |> Dict.toList
        |> List.foldl
            (\ ( path, proxyValue ) root ->
                apply { path = path, value = proxyValue } root
            )
            prop


loadJsonValues : Dict (List Int) RawOutUpdate -> Property msg -> Property msg
loadJsonValues dict prop =
    dict
        |> Dict.toList
        |> List.foldl
            (\ ( path, outUpdate ) root ->
                apply (outUpdate |> swap |> fromPort) root
            )
            prop
    {- Tron.Property.fold
        (\path _ root ->
            Dict.get (Path.toList path) dict
                |> Maybe.map (\outUpdate -> apply (outUpdate |> swap |> fromPort) root)
                |> Maybe.withDefault root
        )
        prop
        prop -}


applyStringValue : String -> Property msg -> Maybe (Property msg)
applyStringValue str prop =
    let
        helper typeStr maybeFn =
            str
                |> fromString typeStr
                |> Result.toMaybe
                |> Maybe.andThen maybeFn
    in
    case prop of
        Nil ->
            helper
                "ghost"
                (\v ->
                    case v of
                        Other ->
                            Just Nil

                        _ ->
                            Nothing
                )

        Number control ->
            helper
                "slider"
                (\v ->
                    case v of
                        FromSlider n ->
                            control
                                |> Control.setValue n
                                |> Number
                                |> Just

                        _ ->
                            Nothing
                )

        Coordinate control ->
            helper
                "xy"
                (\v ->
                    case v of
                        FromXY n ->
                            control
                                |> Control.setValue n
                                |> Coordinate
                                |> Just

                        _ ->
                            Nothing
                )

        Text control ->
            helper
                "text"
                (\v ->
                    case v of
                        FromInput n ->
                            control
                                |> Text.updateText n
                                |> Text
                                |> Just

                        _ ->
                            Nothing
                )

        Color control ->
            helper
                "color"
                (\v ->
                    case v of
                        FromColor n ->
                            control
                                |> Control.setValue n
                                |> Color
                                |> Just

                        _ ->
                            Nothing
                )

        Toggle control ->
            helper
                "toggle"
                (\v ->
                    case v of
                        FromToggle n ->
                            control
                                |> Control.setValue n
                                |> Toggle
                                |> Just

                        _ ->
                            Nothing
                )

        Action control ->
            helper
                "button"
                (\v ->
                    case v of
                        FromButton ->
                            Just <| Action control

                        _ ->
                            Nothing
                )

        Choice focus shape control ->
            helper
                "choice"
                (\v ->
                    case v of
                        FromChoice n ->
                            control
                                |> Nest.select n
                                |> Choice focus shape
                                |> Just

                        _ ->
                            Nothing
                )

        Group _ _ _ ->
            Nothing


encodeRawPath : RawPath -> E.Value
encodeRawPath =
    E.list E.int


encodePath : Path -> E.Value
encodePath =
    Path.toList >> encodeRawPath


encodePropertyAt : RawPath -> Property msg -> RawProperty
encodePropertyAt path property =
    case property of
        Nil ->
            E.object
                [ ( "type", E.string "ghost" )
                , ( "path", encodeRawPath path )
                ]

        Number (Control { min, max, step } val _) ->
            E.object
                [ ( "type", E.string "slider" )
                , ( "path", encodeRawPath path )
                , ( "current", E.float val )
                , ( "min", E.float min )
                , ( "max", E.float max )
                , ( "step", E.float step )
                ]

        Coordinate (Control ( xSpec, ySpec ) ( x, y ) _) ->
            E.object
                [ ( "type", E.string "xy" )
                , ( "path", encodeRawPath path )
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
                , ( "path", encodeRawPath path )
                , ( "current", E.string val )
                ]

        Color (Control _ val _) ->
            E.object
                [ ( "type", E.string "color" )
                , ( "path", encodeRawPath path )
                , ( "current", encodeColor val )
                ]

        Toggle (Control _ val _) ->
            E.object
                [ ( "type", E.string "toggle" )
                , ( "path", encodeRawPath path )
                , ( "current"
                  , E.string <|
                        case val of
                            TurnedOn ->
                                "on"

                            TurnedOff ->
                                "off"
                  )
                ]

        Action _ ->
            E.object
                [ ( "type", E.string "button" )
                , ( "path", encodeRawPath path )
                ]

        Choice _ _ (Control items { form, selected } _) ->
            E.object
                [ ( "type", E.string "choice" )
                , ( "path", encodeRawPath path )
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
                ]

        Group _ _ (Control items { form } _) ->
            E.object
                [ ( "type", E.string "nest" )
                , ( "path", encodeRawPath path )
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
                ]


encodeNested : RawPath -> Array ( Label, Property msg ) -> RawProperty
encodeNested path items =
    E.list
        (\( id, ( label, property ) ) ->
            E.object
                [ ( "index", E.int id )
                , ( "label", E.string label )
                , ( "property"
                  , encodePropertyAt
                        (path ++ [ id ])
                        property
                  )
                ]
        )
    <|
        Array.toIndexedList <|
            items


encode : Property msg -> RawProperty
encode =
    encodePropertyAt []


encodeClientId : HashId -> E.Value
encodeClientId =
    HashId.toString >> E.string


encodeAck : HashId -> Ack
encodeAck =
    Ack << encodeClientId


noClientId : Ack
noClientId = Ack <| E.null


emptyOutUpdate : RawOutUpdate
emptyOutUpdate =
    { path = []
    , value = E.null
    , stringValue = ""
    , labelPath = []
    , type_ = ""
    , client = E.null
    }


{-
   encodeUpdate : Maybe HashId -> Path -> Property msg -> RawUpdate
   encodeUpdate maybeClient path prop =
       let
           ( type_, value ) =
               case prop of
                   Nil ->
                       ( "ghost", E.null )
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


valueDecoder :
    String
    -> D.Decoder ProxyValue -- FIXME: move to ProxyValue
valueDecoder type_ =
    case type_ of
        "ghost" ->
            D.succeed Other

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

        _ ->
            D.succeed Other


fromString
    :  String
    -> String
    -> Result String ProxyValue -- FIXME: move to ProxyValue
fromString type_ str =
    case type_ of
        "ghost" ->
            Ok Other

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

        _ ->
            Err str


{- fromPort :
    RawUpdate
    -> Update -- FIXME: -> Result
fromPort portUpdate =
    fromPort1
        { path = portUpdate.path
        , type_ = portUpdate.type_
        , value = portUpdate.value
        } -}


swap : RawOutUpdate -> RawInUpdate
swap outUpdate =
    { path = outUpdate.path
    , value = outUpdate.value
    , type_ = outUpdate.type_
    }


fromPort : RawInUpdate -> Update
fromPort portUpdate =
    { path = portUpdate.path
    , value =
        D.decodeValue (valueDecoder portUpdate.type_) portUpdate.value
            |> Result.withDefault Other
    }


toProxy : RawOutUpdate -> ProxyValue
toProxy outUpdate =
    fromPort
        (swap outUpdate)
        |> .value


-- TODO:move all below to corresponding controls


encodeColor : Color -> E.Value
encodeColor =
    E.string << Color.colorToHexWithAlpha


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

