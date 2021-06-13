module Constructor.Main exposing (..)


import Browser exposing (Document)
import Array
import Json.Decode as D
import Json.Encode as E
import Color

import Tron exposing (Tron)
import Tron.OfValue as OfValue
import Tron.Builder.Unit as Tron
import Tron.Option as Option
import Tron.Style.Dock as Dock
import Tron.Style.Theme as Theme
import Tron.Path as Path exposing (Path)

import Tron.Core as Tron
import Tron.Property as Property exposing (LabelPath)
import Tron.Control.Nest as Nest
import Tron.Control.Value as V exposing (Value)
import Tron.Expose.Convert as Property
import Tron.Layout as Layout

import Size
import WithTron exposing (ProgramWithTron)

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Dropdown


type Type
    = None
    | Knob
    | XY
    | Color
    | Text
    | Toggle
    | Button
    | Choice
    | Group


type alias Model =
    ( Maybe ( (Path, LabelPath) , Tron Type )
    , Tron ()
    )


type Msg
    = NoOp
    | Save
    | Append
    | SwitchTo (Path, LabelPath) (Tron Type)
    | Edit String E.Value


for : Model -> OfValue.Tron Msg
for =
    Tuple.second
    >> Tron.map (always NoOp)
    >> OfValue.lift


init : Model
init =
    ( Nothing
    , Tron.root
        [
        ]
    )


update : Msg -> Model -> Model
update msg ( current, currentGui ) =
    case msg of
        NoOp ->
            ( current, currentGui )
        Save ->
            ( Nothing
            , case current of
                Just ( ( path, _ ), newProp ) ->
                    currentGui
                        |> Property.replace
                            (\otherPath otherProp ->
                                if Path.toList otherPath == Path.toList path then
                                    newProp |> Tron.toUnit
                                else
                                    otherProp
                            )
                Nothing -> currentGui
            )
        Append ->
            (
                current
                    |> Maybe.map
                        (Tuple.mapSecond
                        <| Property.append
                            ( "test"
                            , create "Button"
                                |> fillTypes
                            )
                        )
            , currentGui
            )
        Edit propName propValue ->
            ( current
                |> Maybe.map (Tuple.mapSecond <| edit propName propValue)
            , currentGui
            )
        SwitchTo path prop ->
            ( Just ( path, prop )
            , currentGui
            )


view : Model -> Html Msg
view ( current, tree ) =
    Html.div
        []
        [ preview
            <| fillTypes
            --<| addGhosts
            <| tree
        , case current of
            Just ( path, currentProp ) ->
                editorFor path currentProp
            Nothing -> Html.div [] []
        ]


fillTypes : Tron () -> Tron Type
fillTypes =
    Property.reflect
        >> Property.map Tuple.first
        >> Property.map valueToType


valueToType : Value -> Type
valueToType v =
    case v of
        V.FromSlider _ -> Knob
        V.FromXY _ -> XY
        V.FromInput _ -> Text
        V.FromToggle _ -> Toggle
        V.FromColor _ -> Color
        V.FromChoice _ -> Choice
        V.FromSwitch _ -> Choice
        V.FromButton -> Button
        V.FromGroup -> Group
        V.Other -> None


typeToString : Type -> String
typeToString t =
    case t of
        Knob -> "Knob"
        XY -> "XY"
        Text -> "Text"
        Toggle -> "Toggle"
        Button -> "Button"
        Color -> "Color"
        Choice -> "Choice"
        Group -> "Nest"
        None -> "None"


editorFor : ( Path, LabelPath ) -> Tron Type -> Html Msg
editorFor ( path, labelPath ) prop =
    Html.div
        []
        [ typesDropdown <| typeOf prop
        , case prop of
            Property.Group _ _ control ->
                Html.div
                    []
                    <| Html.button
                        [ Html.onClick <| Append ]
                        [ Html.text "Append" ]
                    :: (Nest.getItems control
                            |> Array.indexedMap
                                (\idx (label, prop_) ->
                                    editorFor
                                        ( path |> Path.advance idx
                                        , labelPath ++ [ label ]
                                        )
                                        prop_
                                )
                            |> Array.toList)
            _ -> Html.div [] []
        , Html.button
            [ Html.onClick <| Save ]
            [ Html.text "Save" ]
        ]


typeOf : Tron Type -> Type
typeOf =
    Property.get
        >> Maybe.withDefault None


preview : Tron Type -> Html Msg
preview =
    Property.fold3 (\path cell before -> ( path, cell ) :: before) []
        >> List.reverse
        -- |> List.sortBy (Tuple.first >> Path.toList)
        >> List.map (\(path, cell) -> previewCell path cell)
        >> Html.div []


previewCell : ( Path, LabelPath ) -> Tron Type -> Html Msg
previewCell (path, labelPath) prop =
    Html.button
        [ Html.onClick <| SwitchTo (path, labelPath) prop
        , Html.class "edit-cell"
        ]
        [ Html.span [] [ Html.text <| Path.toString path ]
        , Html.span [] [ Html.text <| String.join "/" <| labelPath ]
        , Html.span []
            [ Html.text <| typeToString <| typeOf prop ]
        , Html.span [] [ Html.text "Edit" ]
        ]


edit : String -> E.Value -> Tron Type -> Tron Type
edit name value prop =
    case ( typeOf prop, name ) of
        ( _, "type" ) ->
            prop
                |> edit_ (create >> fillTypes) D.string value
        _ -> prop


edit_ : (a -> Tron Type) -> D.Decoder a -> E.Value -> Tron Type -> Tron Type
edit_ f decoder value prop =
    case value |> D.decodeValue decoder of
        Ok decodedValue ->
            f decodedValue
        Err _ -> prop


create : String -> Tron ()
create s =
    case s of
        "Knob" ->
            Tron.float { min = 0, max = 1, step = 0.01 } 0
        "XY" ->
            Tron.xy
                ( { min = 0, max = 1, step = 0.01 }
                , { min = 0, max = 1, step = 0.01 }
                )
                ( 0, 0 )
        "Text" ->
            Tron.text "foo"
        "Toggle" ->
            Tron.toggle False
        "Color" ->
            Tron.color Color.black
        "Choice" ->
            Tron.choice
                [
                    ( "foo"
                    , Tron.button |> Tron.map (always "foo")
                    )
                ]
                "foo"
        "Button" ->
            Tron.button
        "Nest" ->
            Tron.nest []
        _ -> Tron.none



addGhosts : Tron () -> Tron ()
addGhosts =
    Property.replace
        <| always
        -- it will skip all non-group elements
        <| Property.append ( "_Add", Tron.none )


-- subscriptions : Model -> Sub Msg
-- subscriptions _ = Sub.none


types : List Type
types =
    [ Knob
    , XY
    , Color
    , Text
    , Toggle
    , Button
    , Choice
    , Group
    ]


typesDropdown : Type -> Html Msg
typesDropdown currentType =
    Dropdown.dropdown
        { items =
            types
                |> List.map
                    (\type_ ->
                        { value = typeToString type_
                        , text = typeToString type_
                        , enabled = True
                        }
                    )
        , emptyItem = Nothing
        , onChange = Maybe.withDefault "" >> E.string >> Edit "type"
        }
        []
        (Just <| typeToString currentType)


main : ProgramWithTron () Model Msg
main =
    WithTron.sandbox
        (Option.toHtml Dock.bottomCenter Theme.dark)
        Option.noCommunication
        { for = for
        , init = init
        , view = view
        , update = update
        --, subscriptions = subscriptions
        }