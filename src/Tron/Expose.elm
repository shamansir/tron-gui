module Tron.Expose exposing (..)

-- TODO: make controls expose themselves, so get rid of these imports below

import Array as Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Color.Convert as Color


import Tron.Control as Control exposing (..)
import Tron.Control.Nest as Nest exposing (Form(..))
import Tron.Control.Text as Text exposing (TextState(..))
import Tron.Control.Toggle exposing (ToggleState(..))
import Tron.Control.Button as Button
import Tron.Control.XY as XY
import Tron.Path as Path exposing (Path)
import Tron.Property as Property exposing (..)
import Tron.Control.Value as Value exposing (Value(..))
import Tron.Expose.Data as Exp
import Tron.Expose.Convert as Exp
import Tron.Util as Util
import Task


runProperty : Value -> Property msg -> Cmd msg
runProperty value property =
    case ( property, value ) of
        ( Nil, _ ) ->
            Cmd.none

        ( Number control, FromSlider f ) ->
            control |> Control.update ( Tuple.mapSecond <| always f ) |> Control.run

        ( Coordinate control, FromXY xy ) ->
            control |> Control.update ( Tuple.mapSecond <| always xy ) |> Control.run

        ( Text control, FromInput s ) ->
            control |> Control.update ( Tuple.mapSecond <| always s ) |> Control.run

        ( Color control, FromColor c ) ->
            control |> Control.update ( Tuple.mapSecond <| always c )|> Control.run

        ( Toggle control, FromToggle t ) ->
            control |> setValue t |> Control.run

        ( Action control, FromButton ) ->
            control |> setValue () |> Control.run

        ( Choice _ _ control, FromChoice i ) ->
            control
                |> Control.update
                    (\curValue ->
                        { curValue
                            | selected = i
                        }
                    )
                |> Control.run

        ( Group _ _ _, _ ) ->
            Cmd.none

        ( _, _ ) ->
            Cmd.none


run : Exp.Update -> Property msg -> Cmd msg
run { path, value } prop =
    case path of
        [] ->
            runProperty value prop

        id :: next ->
            case prop of
                Group _ _ control ->
                    case control |> Nest.get id of
                        Just ( _, innerProp ) ->
                            innerProp
                                |> run { path = next, value = value }

                        Nothing ->
                            Cmd.none

                _ ->
                    Cmd.none


applyProperty : Value -> Property a -> Property a
applyProperty value prop =
    case ( prop, value ) of
        ( Nil, _ ) ->
            prop

        ( Number control, FromSlider f ) ->
            control |> Control.update ( Tuple.mapSecond <| always f ) |> Number

        ( Coordinate control, FromXY xy ) ->
            control |> Control.update ( Tuple.mapSecond <| always xy ) |> Coordinate

        ( Text control, FromInput s ) ->
            control |> Control.update ( Tuple.mapSecond <| always s ) |> Text

        ( Color control, FromColor c ) ->
            control |> Control.update ( Tuple.mapSecond <| always c ) |> Color

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


apply : Exp.Update -> Property a -> Property a
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


loadValues : Dict (List Int) Value -> Property a -> Property a
loadValues dict prop =
    dict
        |> Dict.toList
        |> List.foldl
            (\ ( path, value ) root ->
                apply { path = path, value = value } root
            )
            prop

loadStringValues : Dict LabelPath String -> Property a -> Property a
loadStringValues dict prop =
    Property.replaceWithLabeledPath
        (\labelPath innerProp ->
            Dict.get labelPath dict
                |> Maybe.andThen (\strValue -> applyStringValue strValue innerProp)
                |> Maybe.withDefault innerProp
        )
        prop


loadJsonValues : Dict (List Int) Exp.Value -> Property a -> Property a
loadJsonValues dict prop =
    dict
        |> Dict.toList
        |> List.foldl
            (\ ( path, outUpdate ) root ->
                apply (outUpdate |> loadValue |> fromPort) root
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


applyStringValue : String -> Property a -> Maybe (Property a)
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
                                |> Control.update ( Tuple.mapSecond <| always n )
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
                        FromXY xy ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always xy )
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
                        FromColor color ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always color )
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

        {- Switch control ->
            helper
                "switch"
                (\v ->
                    case v of
                        FromSwitch n ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always n )
                                |> Switch
                                |> Just

                        _ ->
                            Nothing
                ) -}

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

        Live innerProp ->
            innerProp
                |> applyStringValue str
                |> Maybe.map Live


decode : D.Decoder (Property ())
decode =
    D.field "type" D.string
    |> D.andThen
        (\typeStr ->
            case typeStr of
                "ghost" -> D.succeed Nil
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
                    D.succeed
                        <| Action
                        <| Control
                            Button.Default
                            ()
                            ()

                "group" ->
                    D.field "nest"
                        (D.array
                            <| D.map2
                                Tuple.pair
                                (D.field "label" D.string)
                                (D.field "property" decode)
                        )
                        |> D.map
                            (\items -> Nest.createGroup items ())
                        |> D.map (Group Nothing defaultNestShape)


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
                        |> D.map (Choice Nothing defaultNestShape)


                _ -> D.succeed Nil -- or fail?
        )


encodeRawPath : List Int -> E.Value
encodeRawPath =
    E.list E.int


encodePath : Path -> E.Value
encodePath =
    Path.toList >> encodeRawPath


encodePropertyAt : List Int -> Property a -> Exp.Property
encodePropertyAt path property =
    case property of
        Nil ->
            E.object
                [ ( "type", E.string "ghost" )
                , ( "path", encodeRawPath path )
                ]

        Number (Control { min, max, step } ( _, val ) _) ->
            E.object
                [ ( "type", E.string "slider" )
                , ( "path", encodeRawPath path )
                , ( "current", E.float val )
                , ( "min", E.float min )
                , ( "max", E.float max )
                , ( "step", E.float step )
                ]

        Coordinate (Control ( xSpec, ySpec ) ( _, ( x, y ) ) _) ->
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

        Color (Control _ ( _, val ) _) ->
            E.object
                [ ( "type", E.string "color" )
                , ( "path", encodeRawPath path )
                , ( "current", encodeColor val )
                , ( "currentRgba", encodeRgba val )
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

        Action (Control face _ _) ->
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

        Live innerProp ->
            encodePropertyAt path innerProp


encodeNested : List Int -> Array ( Label, Property a ) -> Exp.Property
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


encode : Property msg -> Exp.Property
encode =
    encodePropertyAt []


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
    -> D.Decoder Value -- FIXME: move to Value
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

        "nest" ->
            D.succeed FromGroup

        _ ->
            D.succeed Other


fromString
    :  String
    -> String
    -> Result String Value -- FIXME: move to Value
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

        "nest" ->
            Ok FromGroup

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



-- FIXME: move functions below to `Expose.Convert` module? (it is exposed to Public API, so may be not)

swap : Exp.Out -> Exp.In
swap { update } =
    { path = update.path
    , value = update.value
    , type_ = update.type_
    }


loadValue : Exp.Value -> Exp.In
loadValue update =
    { path = update.path
    , value = update.value
    , type_ = update.type_
    }


fromPort : Exp.In -> Exp.Update
fromPort portUpdate =
    { path = portUpdate.path
    , value =
        D.decodeValue (valueDecoder portUpdate.type_) portUpdate.value
            |> Result.withDefault Other
    }


toProxy : Exp.Out -> Value
toProxy outUpdate =
    fromPort
        (swap outUpdate)
        |> .value


-- TODO: move all below to corresponding controls


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


freshRun : Property (Value -> Maybe msg) -> Cmd msg
freshRun =
    Exp.evaluate
    -- >> Property.run
    >> runMaybe


runMaybe : Property (Maybe msg) -> Cmd msg
runMaybe =
    Property.get
    >> Maybe.andThen identity
    >> Util.runMaybe


{-runExposed : Property Exp.Update -> Cmd Exp.Out
runExposed prop =
    case Property.get prop of
        Just rawUpdate ->
            Task.succeed rawUpdate
                |> Task.perform identity
        Nothing -> Cmd.none -}