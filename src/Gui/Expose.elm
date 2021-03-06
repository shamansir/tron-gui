module Gui.Expose exposing (..)

import Color exposing (Color)
import HashId exposing (HashId)

import Json.Decode as D
import Json.Encode as E

import Array as Array
import Array exposing (Array)

import Gui.Util exposing (findMap)
import Gui.Control exposing (..)
import Gui.Control as Control exposing (update)
import Gui.Property exposing (..)
import Gui.Path exposing (Path)
import Gui.Path as Path exposing (toList)
import Gui.ProxyValue as ProxyValue exposing (ProxyValue(..))

-- TODO: make controls expose themselves, so get rid of these imports below
import Gui.Control.Text as Text exposing (TextState(..))
import Gui.Control.Toggle as Toggle exposing (ToggleState(..))
import Gui.Control.Nest as Nest exposing (Form(..))


type alias RawPath = List Int


type alias RawProperty = E.Value


type alias RawClientId = E.Value


type alias Update =
    { path : RawPath
    , value : ProxyValue
    }


type alias RawUpdate =
    { path : RawPath
    , value : E.Value
    , type_ : String
    , client : RawClientId
    }


type alias Ack =
    { client : RawClientId
    }


toProxied : Property msg -> Property ( ProxyValue, msg )
toProxied prop =
    let
        helper : (v -> ProxyValue) -> ( v -> msg -> ( ProxyValue, msg ) )
        -- helper : (a -> b) -> ( a -> c -> ( b, c ) )
        helper toProxy v msg = ( toProxy v, msg )
    in
        case prop of
            Nil ->
                Nil
            Number control ->
                control
                    |> mapWithValue (helper FromSlider)
                    |> Number
            Coordinate control ->
                control
                    |> mapWithValue (helper FromXY)
                    |> Coordinate
            Text control ->
                control
                    |> mapWithValue (helper (Tuple.second >> FromInput))
                    |> Text
            Color control ->
                control
                    |> mapWithValue (helper FromColor)
                    |> Color
            Toggle control ->
                control
                    |> mapWithValue (helper FromToggle)
                    |> Toggle
            Action control ->
                control
                    |> mapWithValue (helper <| always FromButton)
                    |> Action
            Choice focus shape control ->
                control
                    |> Nest.mapItems (Tuple.mapSecond toProxied)
                    |> mapWithValue (helper (.selected >> FromChoice))
                    |> Choice focus shape
            Group focus shape control ->
                control
                    |> Nest.mapItems (Tuple.mapSecond toProxied)
                    |> mapWithValue (helper <| always Other)
                    -- TODO: notify expanded/collapsed/detached?
                    |> Group focus shape


toExposed : Property msg -> Property ( RawUpdate, msg )
toExposed prop =
    prop
        |> toProxied
        |> Gui.Property.addPath
         -- FIXME: `Expose.encodeUpdate` does the same as above
        |> Gui.Property.map
            (\(path, (proxyVal, msg)) ->
                (
                    { path = Path.toList path
                    , type_ = getTypeString proxyVal
                    , value = ProxyValue.encode proxyVal
                    , client = E.null
                    }
                , msg
                )
            )


toStrExposed : Property msg -> Property ( ( String, String ), msg )
toStrExposed prop =
    prop
        |> toProxied
        |> Gui.Property.addLabeledPath
        |> Gui.Property.map
            (\(path, (proxyVal, msg)) ->
                (
                    ( String.join "/" path
                    , ProxyValue.toString proxyVal
                    )
                    , msg
                )
            )


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
                curValue = Control.getValue control
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
                _ -> Cmd.none


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
            control |> Control.setValue (Ready, s) |> Text
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
        [] -> applyProperty value prop
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
                _ -> prop


encodeRawPath : RawPath -> E.Value
encodeRawPath=
    E.list E.int


encodePath : Path -> E.Value
encodePath = Path.toList >> encodeRawPath


encodePropertyAt : RawPath -> Property msg -> RawProperty
encodePropertyAt path property =
    case property of
        Nil ->
            E.object
                [ ( "type", E.string "ghost" )
                , ( "path", encodeRawPath path )
                ]
        Number ( Control { min, max, step } val _ ) ->
            E.object
                [ ( "type", E.string "slider"  )
                , ( "path", encodeRawPath path )
                , ( "current", E.float val )
                , ( "min", E.float min )
                , ( "max", E.float max )
                , ( "step", E.float step )
                ]
        Coordinate ( Control ( xSpec, ySpec ) ( x, y ) _ ) ->
            E.object
                [ ( "type", E.string "xy" )
                , ( "path", encodeRawPath path )
                , ( "current",
                    E.object
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
        Text ( Control _ ( _, val ) _ ) ->
            E.object
                [ ( "type", E.string "text" )
                , ( "path", encodeRawPath path )
                , ( "current", E.string val )
                ]
        Color ( Control _ val _ ) ->
            E.object
                [ ( "type", E.string "color" )
                , ( "path", encodeRawPath path )
                , ( "current", encodeColor val )
                ]
        Toggle ( Control _ val _ ) ->
            E.object
                [ ( "type", E.string "toggle" )
                , ( "path", encodeRawPath path )
                ,
                    ( "current"
                    , E.string
                        <| case val of
                            TurnedOn -> "on"
                            TurnedOff -> "off"
                    )
                ]
        Action _ ->
            E.object
                [ ( "type", E.string "button" )
                , ( "path", encodeRawPath path )
                ]
        Choice _ _ ( Control items { form, selected } _) ->
            E.object
                [ ( "type", E.string "choice" )
                , ( "path", encodeRawPath path )
                ,
                    ( "current"
                    , E.int selected
                    )
                ,
                    ( "expanded"
                    , E.bool
                        <| case form of
                            Expanded -> True
                            Collapsed -> False
                            Detached -> False
                    )
                ,
                    ( "detached"
                    , E.bool
                        <| case form of
                            Detached -> True
                            Collapsed -> False
                            Expanded -> False
                    )
                , ( "options", encodeNested path items )
                ]
        Group _ _ (Control items { form } _) ->
            E.object
                [ ( "type", E.string "nest" )
                , ( "path", encodeRawPath path )
                ,
                    ( "expanded"
                    , E.bool
                        <| case form of
                            Expanded -> True
                            Collapsed -> False
                            Detached -> False
                    )
                ,
                    ( "detached", E.bool <| case form of
                    Detached -> True
                    Collapsed -> False
                    Expanded -> False )
                , ( "nest", encodeNested path items )
                ]


encodeNested : RawPath -> Array ( Label, Property msg ) -> RawProperty
encodeNested path items =
    E.list
        (\(id, (label, property)) ->
            E.object
                [ ( "index", E.int id )
                , ( "label", E.string label )
                , ( "property"
                    , encodePropertyAt
                            ( path ++ [ id ] )
                            property
                    )
                ]
            )
        <| Array.toIndexedList
        <| items


encode : Property msg -> RawProperty
encode = encodePropertyAt []


encodeClientId : Maybe HashId -> E.Value
encodeClientId maybeClient =
    maybeClient |> Maybe.map (HashId.toString >> E.string) |> Maybe.withDefault E.null


encodeAck : Maybe HashId -> Ack
encodeAck maybeClient =
    { client =
        encodeClientId maybeClient
    }


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
                    , E.string <| String.fromFloat x ++ "|" ++ String.fromFloat y
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


-- select : Path -> Gui msg -> Gui msg
-- select selector gui = gui


getTypeString : ProxyValue -> String -- FIXME: move to ProxyValue
getTypeString value =
    case value of
        Other -> "ghost"
        FromSlider _ -> "slider"
        FromXY _ -> "xy"
        FromInput _ -> "text"
        FromColor _ -> "color"
        FromChoice _ -> "choice"
        FromToggle _ -> "toggle"
        FromButton -> "button"


valueDecoder : String -> D.Decoder ProxyValue -- FIXME: move to ProxyValue
valueDecoder type_ =
    case type_ of
        "ghost" -> D.succeed Other
        "slider" -> D.float |> D.map FromSlider
        "xy" -> decodeCoord |> D.map FromXY
        "text" -> D.string |> D.map FromInput
        "color" -> decodeColor |> D.map FromColor
        "choice" -> D.int |> D.map FromChoice
        "toggle" -> decodeToggle |> D.map FromToggle
        "button" -> D.succeed FromButton
        _ -> D.succeed Other



fromPort : RawUpdate -> Update -- FIXME: -> Result
fromPort portUpdate =
    { path = portUpdate.path
    , value =
        D.decodeValue (valueDecoder portUpdate.type_) portUpdate.value
            |> Result.withDefault Other
    }


encodeColor : Color -> E.Value
encodeColor color =
    E.string <| case Color.toRgba color of
        { red, green, blue, alpha } ->
            [ red, green, blue, alpha ]
                |> List.map String.fromFloat
                |> String.join ","


decodeColor : D.Decoder Color
decodeColor =
    D.string
        |> D.andThen (\str ->
            case String.split "," str of
                r::g::b::a::_ ->
                    Maybe.map4
                        Color.rgba
                        (String.toFloat r)
                        (String.toFloat g)
                        (String.toFloat b)
                        (String.toFloat a)
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault (D.fail <| "failed to parse color: " ++ str)
                _ -> D.fail <| "failed to parse color: " ++ str
        )


decodeCoord : D.Decoder (Float, Float)
decodeCoord =
    D.string
        |> D.andThen
        (\str ->
            case str |> String.split "|" of
                v1::v2::_ ->
                    Maybe.map2
                        Tuple.pair
                        (String.toFloat v1)
                        (String.toFloat v2)
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault (D.fail <| "failed to parse coord: " ++ str)
                _ -> D.fail <| "failed to parse coord: " ++ str
        )


decodeToggle : D.Decoder ToggleState
decodeToggle =
    D.string
        |> D.map
        (\str ->
            case str of
                "on" -> TurnedOn
                "off" -> TurnedOff
                _ -> TurnedOff
        )
