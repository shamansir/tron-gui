port module Constructor.Main exposing (..)


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
import WithTron.ValueAt as V
import Tron.Layout as Layout

import Size
import WithTron exposing (ProgramWithTron)

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Dropdown

import Example.Default.Gui as Example_Default
import Example.Default.Model as Example_Default
import Example.Goose.Gui as Example_Goose
import Example.Goose.Model as Example_Goose
import Example.Tiler.Gui as Example_Tiler
import Example.Tiler.Logic as Example_Tiler


import Constructor.ToBuilder as ToBuilder


type Example
    = Empty
    | Goose
    | Tiler
    | Default


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
    | Remove Int
    | SwitchTo (Path, LabelPath) (Tron Type)
    | Edit String E.Value
    | EditLabel String
    | LoadExample Example


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
                            ( "stub"
                            , create "Button"
                                |> fillTypes
                            )
                        )
            , currentGui
            )
        Remove idx ->
            (
                current
                    |> Maybe.map
                        (Tuple.mapSecond <| Property.remove idx)
            , currentGui
            )
        Edit propName propValue ->
            ( current
                |> Maybe.map (Tuple.mapSecond <| edit propName propValue)
            , currentGui
            )
        EditLabel newLabel ->
            case current of
                Just ( ( path, labelPath ), prop ) ->
                    ( Just
                        ( ( path, labelPath |> changeLastTo newLabel ), prop )
                    , currentGui
                        |> Property.changeLabel path newLabel
                    )
                Nothing -> ( current, currentGui )

        SwitchTo path prop ->
            ( Just ( path, prop )
            , currentGui
            )
        LoadExample example ->
            ( Nothing
            , case example of
                Empty -> Tron.root []
                Default -> Example_Default.for Example_Default.default |> Tron.toUnit
                Goose -> Example_Goose.for Example_Goose.default |> Tron.toUnit
                Tiler -> Example_Tiler.gui V.empty (Example_Tiler.init () V.empty |> Tuple.first) |> Tron.toUnit
            )


view : Model -> Html Msg
view ( current, tree ) =
    Html.div
        [ Html.id "constructor" ]
        [ preview
            <| fillTypes
            --<| addGhosts
            <| tree
        , case current of
            Just ( path, currentProp ) ->
                editorFor path currentProp
            Nothing ->
                Html.div
                    [ Html.class "editor", Html.class "editor--empty" ]
                    [ Html.text "Select something" ]

        , Html.div
           [ Html.id "code" ]
           [ viewCode tree ]
        , Html.div
            [ Html.id "examples" ]
            [ Html.button [ Html.onClick <| LoadExample Empty ] [ Html.text "Empty" ]
            , Html.button [ Html.onClick <| LoadExample Goose ] [ Html.text "Goose" ]
            , Html.button [ Html.onClick <| LoadExample Tiler ] [ Html.text "Tiler" ]
            , Html.button [ Html.onClick <| LoadExample Default ] [ Html.text "Default" ]
            ]
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
        [ Html.class "editor" ]
        [ viewPath path
        , viewLabelPath labelPath
        , case labelPath |> List.reverse |> List.head of
            Just label ->
                Html.div
                    [ ]
                    [ Html.input
                        [ Html.type_ "text"
                        , Html.onInput <| EditLabel
                        , Html.placeholder label
                        ]
                        [ ]
                    ]
            Nothing -> Html.span [] []
        , typesDropdown <| typeOf prop
        , case prop of
            Property.Group _ _ control ->
                Html.div
                    []
                    <| (Nest.getItems control
                            |> Array.indexedMap
                                (\idx (label, prop_) ->
                                    previewNestCell
                                    -- editorFor
                                        ( path |> Path.advance idx
                                        , labelPath ++ [ label ]
                                        )
                                        prop_
                                )
                            |> Array.toList)
                    ++ [ Html.button
                        [ Html.onClick <| Append ]
                        [ Html.text "Append" ] ]
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
        >> Html.div [ Html.id "tree" ]


previewCell : ( Path, LabelPath ) -> Tron Type -> Html Msg
previewCell (path, labelPath) prop =
    Html.button
        [ Html.onClick <| SwitchTo (path, labelPath) prop
        , Html.class "edit-cell"
        ]
        [ if Path.howDeep path > 0
            then viewPath path
            else emptyPath
        , if List.length labelPath > 0
            then viewLabelPath labelPath
            else emptyLabelPath
        , Html.span [ Html.class "cell-type" ]
            [ Html.text <| typeToString <| typeOf prop ]
        , Html.span
            [ Html.class "verb" ] [ Html.text "Edit" ]
        ]


previewNestCell : ( Path, LabelPath ) -> Tron Type -> Html Msg
previewNestCell (path, labelPath) prop =
    Html.button
        [ Html.class "edit-cell"
        , Html.class "edit-cell--preview"
        ]
        [ if Path.howDeep path > 0
            then viewPath path
            else emptyPath
        , if List.length labelPath > 0
            then viewLabelPath labelPath
            else emptyLabelPath
        , Html.span [ Html.class "cell-type" ]
            [ Html.text <| typeToString <| typeOf prop ]
        , Html.span
            [ Html.class "verb verb--danger"
            , Html.onClick <| case Path.pop path |> Maybe.map Tuple.second of
                Just idx -> Remove idx
                Nothing -> NoOp
            ]
            [ Html.text "Remove" ]
        ]


emptyPath : Html msg
emptyPath =
    Html.span
        [ Html.class "path" ]
        [ Html.span
            [ Html.class "empty" ]
            [ Html.text "-" ]
        ]


emptyLabelPath : Html msg
emptyLabelPath =
    Html.span
        [ Html.class "label-path" ]
        [ Html.span
            [ Html.class "empty" ]
            [ Html.text "-" ]
        ]


viewPath : Path -> Html msg
viewPath =
    Path.toList
        >> List.map String.fromInt
        >> List.map (\n -> Html.span [ Html.class "item" ] [ Html.text n ])
        >> List.intersperse (Html.span [ Html.class "sep" ] [ Html.text "/" ])
        >> Html.span [ Html.class "path" ]


viewLabelPath : LabelPath -> Html msg
viewLabelPath =
     List.map (\n -> Html.span [ Html.class "item" ] [ Html.text n ])
        >> List.intersperse (Html.span [ Html.class "sep" ] [ Html.text "/" ])
        >> Html.span [ Html.class "label-path" ]


changeLastTo : String -> LabelPath -> LabelPath
changeLastTo newLabel path =
    let
        reversed = List.reverse path
    in
        (case reversed |> List.head of
            Just _ ->
                newLabel :: (List.tail reversed |> Maybe.withDefault [])
            Nothing ->
                []
        ) |> List.reverse


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


viewCode : Tron () -> Html msg
viewCode =
    ToBuilder.toCodeLines
        >> String.join "\n"
        >> Html.text
        >> List.singleton
        >> Html.textarea [ Html.id "builder-code" ]


addGhosts : Tron () -> Tron ()
addGhosts =
    Property.replace
        <| always
        -- it will skip all non-group elements
        <| Property.append ( "_Add", Tron.none )


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


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
    WithTron.element
        (Option.toHtml Dock.bottomCenter Theme.dark)
        Option.noCommunication
        { for = for
        , init = \_ -> ( init, Cmd.none )
        , view = view
        , update =
            (\msg model ->
                ( update msg model
                , case msg of
                    LoadExample _ ->
                        updateCodeMirror ()
                    Save ->
                        updateCodeMirror ()
                    _ -> Cmd.none
                )
            )
        , subscriptions = subscriptions
        }


port updateCodeMirror : () -> Cmd msg