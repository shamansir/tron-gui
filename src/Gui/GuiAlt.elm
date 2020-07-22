module Gui.GuiAlt exposing (..)

import Json.Encode as E

import Gui.Util exposing (findMap)


type alias Label = String


type alias Color = String


type alias Path = List { item : String, index : Int }


type alias Error = String


type ToggleState
    = On
    | Off


type Option = Option String -- means it should be unique


type ExpandState
    = Collapsed
    | Expanded


type alias Gui msg
    = List ( Label, Property msg )


type alias Update =
    { path : Path
    , value : String
    }


type Property msg
    = Ghost
    | Slider { min: Float, max : Float, step : Float } Float ( Float -> msg )
    | Input String ( String -> msg )
    | Choice ( List ( Label, Option ) ) ( Maybe Option ) ( Option -> msg )
    | Color Color ( Color -> msg )
    | Toggle ToggleState ( ToggleState -> msg )
    | Button ( () -> msg )
    | Nested ExpandState ( Gui msg )


updateProperty : String -> Property msg -> Maybe msg
updateProperty value property
    = case property of
        Ghost ->
            Nothing
        Slider _ _ toMsg ->
            String.toFloat value |> Maybe.map toMsg
        Input _ toMsg ->
            Just <| toMsg value
        Choice options maybeDefault toMsg ->
            findMap
                (\( _, Option option ) ->
                    if value == option
                        then Just <| toMsg <| Option value
                        else Nothing
                )
                options
        Color _ toMsg ->
            Just <| toMsg value
        Toggle _ toMsg ->
            case value of
                "on" -> Just <| toMsg On
                "off" -> Just <| toMsg Off
                _ -> Nothing
        Button toMsg ->
            Just <| toMsg ()
        Nested _ _ ->
            Nothing


update : Update -> Gui msg -> Maybe msg
update { path, value } gui =
    case path of
        [] -> Nothing
        { item, index } :: next ->
            gui
                |> List.indexedMap (Tuple.pair)
                |> findMap
                (\( otherIndex, ( otherLabel, property ) ) ->
                    if (item == otherLabel) && (index == otherIndex) then
                            case property of
                                Nested _ innerGui ->
                                    update { path = next, value = value } innerGui
                                directProperty ->
                                    updateProperty value directProperty
                        else Nothing
                )


encodePath : Path -> E.Value
encodePath=
    E.list
        (\{ item, index } ->
            E.object
                [ ( "item", E.string item )
                , ( "index", E.int index )
                ]
        )


encodePropertyAt : Path -> Property msg -> E.Value
encodePropertyAt path property =
    case property of
        Ghost ->
            E.object
                [ ( "type", E.string "ghost" )
                , ( "path", encodePath path )
                ]
        Slider { min, max, step } val _ ->
            E.object
                [ ( "type", E.string "slider"  )
                , ( "path", encodePath path )
                , ( "current", E.float val )
                , ( "min", E.float min )
                , ( "max", E.float max )
                , ( "step", E.float step )
                ]
        Input val _ ->
            E.object
                [ ( "type", E.string "text" )
                , ( "path", encodePath path )
                , ( "current", E.string val )
                ]
        Choice options maybeVal _ ->
            E.object
                [ ( "type", E.string "choice" )
                , ( "path", encodePath path )
                , ( "current", maybeVal
                    |> Maybe.map (optionToString >> E.string)
                    |> Maybe.withDefault E.null )
                , ( "options", E.list
                        (\(index, (label, Option option)) ->
                            E.object
                                [ ( "index", E.int index )
                                , ( "label", E.string label )
                                , ( "value", E.string option )
                                ]
                        )
                        <| List.indexedMap Tuple.pair options )
                ]
        Color val _ ->
            E.object
                [ ( "type", E.string "color" )
                , ( "path", encodePath path )
                , ( "current", E.string val )
                ]
        Toggle val _ ->
            E.object
                [ ( "type", E.string "toggle" )
                , ( "path", encodePath path )
                , ( "current", E.string
                        <| case val of
                            On -> "on"
                            Off -> "off" )
                ]
        Button _ ->
            E.object
                [ ( "type", E.string "button" )
                , ( "path", encodePath path )
                ]
        Nested expanded gui ->
            E.object
                [ ( "type", E.string "gui" )
                , ( "path", encodePath path )
                , ( "expanded", E.bool <| case expanded of
                    Expanded -> True
                    Collapsed -> False )
                , ( "nest", encodeAt path gui )
                ]


encodeAt : Path -> Gui msg -> E.Value
encodeAt path gui =
    E.object
        [
            ( "gui"
            , E.list
                (\(index, ( label, property) ) ->
                    E.object
                        [ ( "index", E.int index )
                        , ( "label", E.string label )
                        , ( "property"
                            , encodePropertyAt
                                    ( path ++ [ { item = label, index = index } ] )
                                    property
                            )
                        ]
                    )
                <| List.indexedMap Tuple.pair gui
            )
        ]


encode : Gui msg -> E.Value
encode = encodeAt []


ghost : Property msg
ghost = Ghost


float : { min: Float, max : Float, step : Float } -> Float -> ( Float -> msg ) -> Property msg
float = Slider


int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Property msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step }
        (toFloat default)
        (round >> toMsg)


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Property msg
input toString fromString default toMsg =
    Input
        (toString default)
        (fromString >> Maybe.withDefault default >> toMsg)


text : String -> (String -> msg) -> Property msg
text = Input


-- numberInput : Float -> (Float -> msg) -> Property msg
-- numberInput = input String.fromFloat String.toFloat


color : Color -> (Color -> msg) -> Property msg
color = Color


choice
     : ( String -> Result Error a )
    -> ( a -> ( Label, Option ) )
    -> List a
    -> Maybe a
    -> ( ( Option, Error ) -> msg )
    -> ( a -> msg )
    -> Property msg
choice decoder toOption options maybeSelected onError toMsg =
    Choice
        (options |> List.map toOption)
        (maybeSelected |> Maybe.map (toOption >> Tuple.second))
        (withFallback decoder onError toMsg)


strings
     : List String
    -> Maybe String
    -> ( String -> msg )
    -> Property msg
strings options maybeSelected toMsg =
    Choice
        (options |> List.map Option |> List.map labelFromValue)
        (maybeSelected |> Maybe.map Option)
        (optionToString >> toMsg)


toggle : ToggleState -> (ToggleState -> msg) -> Property msg
toggle = Toggle


button : (() -> msg) -> Property msg
button = Button


nest : ExpandState -> Gui msg -> Property msg
nest = Nested


none : Property msg
none = Ghost


map : ( msgA -> msgB ) -> Gui msgA -> Gui msgB
map = List.map << Tuple.mapSecond << mapProperty


mapProperty : ( msgA -> msgB ) -> Property msgA -> Property msgB
mapProperty f prop =
    case prop of
        Ghost -> Ghost
        Slider opts val toMsg -> Slider opts val (f << toMsg)
        Input val toMsg -> Input val (f << toMsg)
        Color val toMsg -> Color val (f << toMsg)
        Toggle val toMsg -> Toggle val (f << toMsg)
        Button toMsg -> Button (f << toMsg)
        Nested state gui -> Nested state (map f gui)
        Choice vals options toMsg -> Choice vals options (f << toMsg)


withFallback
     : ( String -> Result Error a )
    -> ( ( Option, Error ) -> msg )
    -> ( a -> msg ) -> ( Option -> msg )
withFallback decode onError toMsg =
    \(Option option) ->
        case decode option of
            Ok val -> toMsg val
            Err error -> onError ( Option option, error )


labelFromValue : Option -> ( Label, Option )
labelFromValue (Option value) =
    ( value, Option value )


optionToString : Option -> String
optionToString (Option option) = option


toggleToBool : ToggleState -> Bool
toggleToBool state =
    case state of
        On -> True
        Off -> False


boolToToggle : Bool -> ToggleState
boolToToggle bool =
    case bool of
        True -> On
        False -> Off


select : Path -> Gui msg -> Gui msg
select selector gui = gui
