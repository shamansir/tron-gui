module Gui.Expose exposing (..)

import Json.Decode as D
import Json.Encode as E

import Array as Array
import Array exposing (Array)

import Gui.Util exposing (findMap)
import Gui.Control exposing (..)
import Gui.Property exposing (..)


type alias Path = List Id


type alias Id = Int


type Value
    = FromSlider Float
    | FromInput String
    | FromChoice Id
    | FromColor Color
    | FromToggle ToggleState
    | FromButton
    | Other


type alias Update =
    { path : Path
    , value : Value
    }


type alias PortUpdate =
    { path : Path
    , value : E.Value
    , type_ : String
    }


updateProperty : Value -> Property msg -> Cmd msg
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
        ( Choice ( Control _ ( expanded, _ ) _ as control), FromChoice i ) ->
            ( expanded, i ) |> callWith control
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


encodePath : Path -> E.Value
encodePath=
    E.list E.int


encodePropertyAt : Path -> Property msg -> E.Value
encodePropertyAt path property =
    case property of
        Nil ->
            E.object
                [ ( "type", E.string "ghost" )
                , ( "path", encodePath path )
                ]
        Number ( Control { min, max, step } val _ ) ->
            E.object
                [ ( "type", E.string "slider"  )
                , ( "path", encodePath path )
                , ( "current", E.float val )
                , ( "min", E.float min )
                , ( "max", E.float max )
                , ( "step", E.float step )
                ]
        Coordinate ( Control ( xSpec, ySpec ) ( x, y ) _ ) ->
            E.object
                [ ( "type", E.string "xy"  )
                , ( "path", encodePath path )
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
                , ( "path", encodePath path )
                , ( "current", E.string val )
                ]
        Color ( Control _ val _ ) ->
            E.object
                [ ( "type", E.string "color" )
                , ( "path", encodePath path )
                , ( "current", E.string val )
                ]
        Toggle ( Control _ val _ ) ->
            E.object
                [ ( "type", E.string "toggle" )
                , ( "path", encodePath path )
                , ( "current", E.string
                        <| case val of
                            TurnedOn -> "on"
                            TurnedOff -> "off" )
                ]
        Action _ ->
            E.object
                [ ( "type", E.string "button" )
                , ( "path", encodePath path )
                ]

        Choice ( Control ( _, items ) ( expanded, current ) _) ->
            E.object
                [ ( "type", E.string "choice" )
                , ( "path", encodePath path )
                , ( "current", E.int current )
                , ( "expanded", E.bool <| case expanded of
                    Expanded -> True
                    Collapsed -> False )
                , ( "options", encodeNested path items )
                ]
        Group (Control ( _, items ) ( expanded, _ ) _ ) ->
            E.object
                [ ( "type", E.string "nest" )
                , ( "path", encodePath path )
                , ( "expanded", E.bool <| case expanded of
                    Expanded -> True
                    Collapsed -> False )
                , ( "nest", encodeNested path items )
                ]


encodeNested : Path -> Array ( Label, Property msg ) -> E.Value
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


encode : Property msg -> E.Value
encode = encodePropertyAt []


-- select : Path -> Gui msg -> Gui msg
-- select selector gui = gui


valueDecoder : String -> D.Decoder Value
valueDecoder type_ =
    case type_ of
        "ghost" -> D.succeed Other
        "slider" -> D.float |> D.map FromSlider
        "text" -> D.string |> D.map FromInput
        "color" -> D.string |> D.map FromColor
        "choice" -> D.int |> D.map FromChoice
        "toggle" -> D.bool |> D.map boolToToggle |> D.map FromToggle
        "button" -> D.succeed FromButton
        _ -> D.succeed Other



fromPort : PortUpdate -> Update -- FIXME: -> Result
fromPort portUpdate =
    { path = portUpdate.path
    , value =
        D.decodeValue (valueDecoder portUpdate.type_) portUpdate.value
            |> Result.withDefault Other
    }
