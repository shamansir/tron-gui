module Gui.Alt exposing (..)

import Json.Decode as D
import Json.Encode as E

import Gui.Util exposing (findMap)
import Process exposing (Id)


type alias Color = String


type alias Path = List Id


type alias Id = Int


type alias Label = String


type ToggleState
    = On
    | Off


type ExpandState
    = Collapsed
    | Expanded


type Property msg
    = Ghost
    | Slider { min: Float, max : Float, step : Float } Float ( Float -> msg )
    | Input String ( String -> msg )
    | Choice ( List ( Id, Label ) ) ( Maybe Id ) ( Id -> Maybe msg )
    | Color Color ( Color -> msg )
    | Toggle ToggleState ( ToggleState -> msg )
    | Button ( () -> msg )
    | Nested ExpandState ( Gui msg )


type alias Gui msg
    = List ( Id, Label, Property msg )


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


updateProperty : Value -> Property msg -> Maybe msg
updateProperty value property
    = case ( property, value ) of
        ( Ghost, _ ) ->
            Nothing
        ( Slider _ _ toMsg, FromSlider f ) ->
            Just <| toMsg f
        ( Input _ toMsg, FromInput s ) ->
            Just <| toMsg s
        ( Choice options maybeDefault toMsg, FromChoice chosenId ) ->
            findMap
                (\( id, _ ) ->
                    if chosenId == id
                        then toMsg id
                        else Nothing
                )
                options
        ( Color _ toMsg, FromColor c ) ->
            Just <| toMsg c
        ( Toggle _ toMsg, FromToggle t ) ->
            Just <| toMsg t
        ( Button toMsg, FromButton ) ->
            Just <| toMsg ()
        ( Nested _ _, _ ) ->
            Nothing
        _ -> Nothing


update : Update -> Gui msg -> Maybe msg
update { path, value } gui =
    case path of
        [] -> Nothing
        id :: next ->
            gui
                |> findMap
                (\( otherId, _, property ) ->
                    if id == otherId then
                        case property of
                            Nested _ innerGui ->
                                update { path = next, value = value } innerGui
                            directProperty ->
                                updateProperty value directProperty
                    else Nothing
                )


encodePath : Path -> E.Value
encodePath=
    E.list E.int


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
                    |> Maybe.map E.int
                    |> Maybe.withDefault E.null )
                , ( "options", E.list
                        (\(index, label) ->
                            E.object
                                [ ( "index", E.int index )
                                , ( "label", E.string label )
                                ]
                        )
                        options )
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
                (\(id, label, property) ->
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
                gui
            )
        ]


encode : Gui msg -> E.Value
encode = encodeAt []


make : List ( Label, Property msg ) -> Gui msg
make = List.indexedMap (\index ( label, prop ) -> (index, label, prop))


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
     : ( a -> Label )
    -> List a
    -> Maybe a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Property msg
choice toLabel options maybeCurrent compare toMsg =
    let
        indexedOptions =
            options |> List.indexedMap Tuple.pair
    in
        Choice
            (indexedOptions |> List.map (Tuple.mapSecond toLabel))
            (maybeCurrent
                |> Maybe.andThen (\current ->
                    indexedOptions
                        |> findMap
                            (\(index, option) ->
                                if compare option current then
                                    Just index else Nothing
                            )
                )
            )
            (\selectedIndex ->
                indexedOptions
                    |> findMap
                        (\(index, option) ->
                            if selectedIndex == index
                                then Just option
                                else Nothing
                        )
                    |> Maybe.map toMsg
            )


strings
     : List String
    -> Maybe String
    -> ( String -> msg )
    -> Property msg
strings options maybeCurrent toMsg =
    choice
        identity
        options
        maybeCurrent
        ((==))
        toMsg


toggle : ToggleState -> (ToggleState -> msg) -> Property msg
toggle = Toggle


button : (() -> msg) -> Property msg
button = Button


nest : ExpandState -> Gui msg -> Property msg
nest = Nested


none : Property msg
none = Ghost


map : ( msgA -> msgB ) -> Gui msgA -> Gui msgB
map f =
    List.map
        (\(id, label, prop) -> (id, label, mapProperty f prop))


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
        Choice vals options toMsg -> Choice vals options (Maybe.map f << toMsg)


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
