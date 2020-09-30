module Gui.Expose exposing (..)

import Color exposing (Color)

import Json.Decode as D
import Json.Encode as E

import Array as Array
import Array exposing (Array)

import Gui.Util exposing (findMap)
import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Path exposing (Path)
import Gui.Path as Path exposing (toList)


type alias RawPath = List Id


type alias RawProperty = E.Value


type alias Id = Int


type ProxyValue -- TODO: get rid of?
    = FromSlider Float
    | FromXY ( Float, Float )
    | FromInput String
    | FromChoice Id
    | FromColor Color
    | FromToggle ToggleState
    | FromButton
    | Other


type alias Update =
    { path : RawPath
    , value : ProxyValue
    }


type alias RawUpdate =
    { path : RawPath
    , value : E.Value
    , type_ : String
    }


updateProperty : ProxyValue -> Property msg -> Cmd msg
updateProperty value property
    = case ( property, value ) of
        ( Nil, _ ) ->
            Cmd.none
        ( Number control, FromSlider f ) ->
            f |> callWith control
        ( Text control, FromInput s ) ->
            s |> callWith control
        ( Color control, FromColor c ) ->
            c |> callWith control
        ( Toggle control, FromToggle t ) ->
            t |> callWith control
        ( Action control, FromButton ) ->
            () |> callWith control
        ( Choice ( Control _ ( expanded, ( focus, _ ) ) _ as control), FromChoice i ) ->
            ( expanded, ( focus, Selected i ) )
                |> callWith control
        ( Group _, _ ) ->
            Cmd.none
        _ -> Cmd.none


update : Update -> Property msg -> Cmd msg
update { path, value } gui =
    case path of
        [] -> updateProperty value gui
        id :: next ->
            case gui of
                Group ( Control ( _, items ) _ _) ->
                    case Array.get id items of
                        Just ( _, innerGui ) ->
                            innerGui
                                |> update { path = next, value = value }
                        Nothing ->
                            Cmd.none
                _ -> Cmd.none


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
        Text ( Control _ val _ ) ->
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
                , ( "current", E.string
                        <| case val of
                            TurnedOn -> "on"
                            TurnedOff -> "off" )
                ]
        Action _ ->
            E.object
                [ ( "type", E.string "button" )
                , ( "path", encodeRawPath path )
                ]

        Choice ( Control ( _, items ) ( state, ( _, Selected current ) ) _) ->
            E.object
                [ ( "type", E.string "choice" )
                , ( "path", encodeRawPath path )
                , ( "current", E.int current )
                , ( "expanded", E.bool <| case state of
                    Expanded -> True
                    Collapsed -> False
                    Detached -> False )
                , ( "detached", E.bool <| case state of
                    Detached -> True
                    Collapsed -> False
                    Expanded -> False )
                , ( "options", encodeNested path items )
                ]
        Group (Control ( _, items ) ( state, _ ) _ ) ->
            E.object
                [ ( "type", E.string "nest" )
                , ( "path", encodeRawPath path )
                , ( "expanded", E.bool <| case state of
                    Expanded -> True
                    Collapsed -> False
                    Detached -> False )
                , ( "detached", E.bool <| case state of
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


encodeUpdate : Path -> Property msg -> RawUpdate
encodeUpdate path prop =
    case prop of
        Nil ->
            { path = Path.toList path
            , value = E.null
            , type_ = "ghost"
            }
        Number ( Control { min, max, step } val _ ) ->
            { path = Path.toList path
            , value = E.float val
            , type_ = "slider"
            }
        Coordinate ( Control ( xSpec, ySpec ) ( x, y ) _ ) ->
            { path = Path.toList path
            , value = E.string <| String.fromFloat x ++ "|" ++ String.fromFloat y
            , type_ = "coord"
            }
        Text ( Control _ val _ ) ->
            { path = Path.toList path
            , value = E.string val
            , type_ = "text"
            }
        Color ( Control _ val _ ) ->
            { path = Path.toList path
            , value = encodeColor val
            , type_ = "color"
            }
        Toggle ( Control _ val _ ) ->
            { path = Path.toList path
            , value = E.string
                        <| case val of
                            TurnedOn -> "on"
                            TurnedOff -> "off"
            , type_ = "toggle"
            }
        Action _ ->
            { path = Path.toList path
            , value = E.null
            , type_ = "button"
            }
        Choice ( Control ( _, items ) ( state, ( _, Selected current ) ) _) ->
            { path = Path.toList path
            , value = E.int current
            , type_ = "choice"
            }
        Group _ ->
            { path = Path.toList path
            , value = E.null
            , type_ = "nest"
            }


-- select : Path -> Gui msg -> Gui msg
-- select selector gui = gui


valueDecoder : String -> D.Decoder ProxyValue
valueDecoder type_ =
    case type_ of
        "ghost" -> D.succeed Other
        "slider" -> D.float |> D.map FromSlider
        "coord" -> decodeCoord |> D.map FromXY
        "text" -> D.string |> D.map FromInput
        "color" -> decodeColor |> D.map FromColor
        "choice" -> D.int |> D.map FromChoice
        "toggle" -> D.bool |> D.map boolToToggle |> D.map FromToggle
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
            {-
            ([ red, green, blue ]
                |> List.map ((*) 255)
                |> List.map String.fromFloat
                |> String.join ",") ++ "," ++ String.fromFloat alpha
            -}


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

{-
encodeColor : Color -> E.Value
encodeColor color =
    E.string <| case Color.toHsla color of
        { hue, saturation, lightness, alpha } ->
            [ hue, saturation, lightness, alpha ]
                |> List.map String.fromFloat
                |> String.join "|"


decodeColor : D.Decoder Color
decodeColor =
    D.string
        |> D.andThen (\str ->
            case String.split "|" str of
                h::s::l::a::_ ->
                    Maybe.map4
                        Color.hsla
                        (String.toFloat h)
                        (String.toFloat s)
                        (String.toFloat l)
                        (String.toFloat a)
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault (D.fail <| "failed to parse color" ++ str)
                _ -> D.fail <| "failed to parse color" ++ str
        )
-}
