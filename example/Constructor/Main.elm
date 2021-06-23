port module Constructor.Main exposing (..)


import Browser exposing (Document)
import Array
import Json.Decode as D
import Json.Encode as E
import Color
import Dict exposing (Dict)

import Tron exposing (Tron)
import Tron.OfValue as OfValue
import Tron.Builder.Unit as Tron
import Tron.Option as Option
import Tron.Style.Dock as Dock
import Tron.Style.Theme as Theme
import Tron.Path as Path exposing (Path)

import Tron.Core as Tron
import Tron.Property as Property exposing (LabelPath)
import Tron.Control as Core
import Tron.Control.Nest as Nest
import Tron.Control.Value as V exposing (Value)
import Tron.Control.Button as Button
import Tron.Expose.Convert as Property
import WithTron.ValueAt as V
import Tron.Expose.Data as Exp
import Tron.Expose as Exp
import Tron.Style.PanelShape as PS
import Tron.Style.CellShape as CS

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
import Constructor.Selector exposing (..)


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


type alias Model =
    { current : Maybe ( (Path, LabelPath) , Tron Type )
    , expands : Dict (List Int) Bool
    , tree : Tron ()
    }


type Msg
    = NoOp
    | Save
    | Append
    | Forward Int
    | Backward Int
    | Remove Int
    | SwitchTo (Path, LabelPath) (Tron Type)
    | Edit String E.Value
    | EditLabel String
    | EditPanelShape PS.PanelShape
    | EditCellShape CS.CellShape
    | EditChoiceMode Nest.ChoiceMode
    | LoadExample Example
    | ToLocalStorage
    | TriggerFromLocalStorage
    | FromLocalStorage (Tron ())
    | ExpandOrCollapse Path


for : Model -> OfValue.Tron Msg
for =
    .tree
    >> Tron.map (always NoOp)
    >> OfValue.lift


init : Model
init =
    { current = Nothing
    , tree =
        Tron.root
            [
            ]
    , expands =
        Dict.empty
            |> Dict.insert [] True
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
        Save ->
            { model
            | current = Nothing
            , tree = case model.current of
                Just ( ( path, _ ), newProp ) ->
                    model.tree
                        |> Property.replace
                            (\otherPath otherProp ->
                                if Path.toList otherPath == Path.toList path then
                                    newProp |> Tron.toUnit
                                else
                                    otherProp
                            )
                Nothing -> model.tree
            }
        Append ->
            { model
            | current =
                model.current
                    |> Maybe.map
                        (Tuple.mapSecond
                        <| Property.append
                            ( "stub"
                            , create "Button"
                                |> fillTypes
                            )
                        )
            }
        Remove idx ->
            { model
            | current =
                model.current
                    |> Maybe.map
                        (Tuple.mapSecond <| Property.remove idx)
            }
        Forward idx ->
            { model
            | current =
                model.current
                    |> Maybe.map
                        (Tuple.mapSecond <| Property.forward idx)
            }
        Backward idx ->
            { model
            | current =
                model.current
                    |> Maybe.map
                        (Tuple.mapSecond <| Property.backward idx)
            }
        Edit propName propValue ->
            { model
            | current =
                model.current
                |> Maybe.map (Tuple.mapSecond <| edit propName propValue)
            }
        EditCellShape newCellShape ->
            { model
            | current =
                model.current
                |> Maybe.map (Tuple.mapSecond <| Property.setCellShape newCellShape)
            }
        EditPanelShape newPanelShape ->
            { model
            | current =
                model.current
                |> Maybe.map (Tuple.mapSecond <| Property.setPanelShape newPanelShape)
            }
        EditChoiceMode newChoiceMode ->
            { model
            | current =
                model.current
                    |> Maybe.map (Tuple.mapSecond <| Property.setChoiceMode newChoiceMode)
            }
        EditLabel newLabel ->
            case model.current of
                Just ( ( path, labelPath ), prop ) ->
                    { model
                    | current =
                        Just
                            ( ( path, labelPath |> changeLastTo newLabel ), prop )
                    , tree =
                        model.tree |> Property.changeLabel path newLabel
                    }
                Nothing -> model

        SwitchTo path prop ->
            { model
            | current = Just ( path, prop )
            }
        LoadExample example ->
            { model
            | current = Nothing
            , expands = Dict.empty |> Dict.insert [] True
            , tree = case example of
                Empty -> Tron.root []
                Default -> Example_Default.for Example_Default.default |> Tron.toUnit
                Goose -> Example_Goose.for Example_Goose.default |> Tron.toUnit
                Tiler -> Example_Tiler.gui V.empty (Example_Tiler.init () V.empty |> Tuple.first) |> Tron.toUnit
            }
        ToLocalStorage ->
            model
        TriggerFromLocalStorage ->
            model
        FromLocalStorage nextGui ->
            { model
            | tree = nextGui
            }
        ExpandOrCollapse path ->
            { model
            | expands
                = model.expands
                    |> Dict.update
                        (Path.toList path)
                        (\maybeCur ->
                            case maybeCur of
                                Just True -> Just False
                                Just False -> Just True
                                Nothing -> Just True
                        )
            }


view : Model -> Html Msg
view model =
    Html.div
        [ Html.id "constructor" ]
        [ Html.div
            [ Html.id "tree" ]
            [ preview
                model.expands
                (model.current
                    |> Maybe.map (Tuple.first >> Tuple.first)
                    |> Maybe.withDefault Path.start
                )
                <| fillTypes
                --<| addGhosts
                <| model.tree
            ]
        , case model.current of
            Just ( path, currentProp ) ->
                editorFor path currentProp
            Nothing ->
                Html.div
                    [ Html.class "editor", Html.class "editor--empty" ]
                    [ Html.text "Select something" ]

        , Html.div
            [ Html.id "code" ]
            [ Html.div
                [ Html.id "examples" ]
                [ Html.button [ Html.onClick <| LoadExample Empty ] [ Html.text "Empty" ]
                , Html.button [ Html.onClick <| LoadExample Goose ] [ Html.text "Goose" ]
                , Html.button [ Html.onClick <| LoadExample Tiler ] [ Html.text "Tiler" ]
                , Html.button [ Html.onClick <| LoadExample Default ] [ Html.text "Default" ]
                , Html.button [ Html.onClick <| ToLocalStorage ] [ Html.text "Save" ]
                , Html.button [ Html.onClick <| TriggerFromLocalStorage ] [ Html.text "Load" ]
                ]
            , viewCode model.tree
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
    let

        shapeEditor ( panelShape, cellShape ) =
            Html.div
                []
                [ viewPanelShapeSelector panelShape EditPanelShape
                , viewCellShapeSelector cellShape EditCellShape
                ]

        itemsEditor items =
            items
                |> Array.indexedMap
                    (\idx (label, prop_) ->
                        previewNestCell
                        -- editorFor
                            ( path |> Path.advance idx
                            , labelPath ++ [ label ]
                            )
                            prop_
                    )
                |> Array.toList
                |> Html.div [Html.class "cell-editor"]

        faceEditor face =
            Html.div
                []
                [ Html.text "Face:"
                , Html.button
                    [ Html.onClick <| Edit "auto" <| E.null ]
                    [ Html.text "Auto" ]
                , Html.button
                    [ Html.onClick <| Edit "color" <| E.null ]
                    [ Html.text "Color" ]
                , Html.button
                    [ Html.onClick <| Edit "icon" <| E.list E.string [] ]
                    [ Html.text "Clear icon" ]
                , viewIconSelector
                    (case face of
                        Just (Button.WithIcon (Button.Icon iconFn)) ->
                            Just <| iconFn Theme.Dark
                        _ -> Nothing
                    )
                    (E.list E.string >> Edit "icon")
                ]

        choiceModeEditor =
            Html.div
                []
                [ Html.text "Mode:"
                , Html.button
                    [ Html.onClick <| EditChoiceMode Nest.Pages ]
                    [ Html.text "Pages" ]
                , Html.button
                    [ Html.onClick <| EditChoiceMode Nest.Knob ]
                    [ Html.text "Knob" ]
                , Html.button
                    [ Html.onClick <| EditChoiceMode Nest.SwitchThrough ]
                    [ Html.text "Switch" ]
                ]

    in
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
            Property.Action (Core.Control face _ _) ->
                faceEditor <| Just face

            Property.Group _ shape (Core.Control _ { face } _ as control) ->
                Html.div
                    []
                    [ faceEditor <| face
                    , itemsEditor <| Nest.getItems control
                    , Html.button
                        [ Html.onClick <| Append ]
                        [ Html.text "Append" ]
                    , shapeEditor shape
                    ]

            Property.Choice _ shape (Core.Control _ { face } _ as control) ->
                Html.div
                    []
                    [ faceEditor <| face
                    , choiceModeEditor
                    , itemsEditor <| Nest.getItems control
                    , Html.button
                        [ Html.onClick <| Append ]
                        [ Html.text "Append" ]
                    , shapeEditor shape
                    ]

            _ -> Html.div [] []

        , Html.button
            [ Html.onClick <| Save ]
            [ Html.text "Save" ]
        ]


typeOf : Tron Type -> Type
typeOf =
    Property.get
        >> Maybe.withDefault None


preview : Dict (List Int) Bool -> Path -> Tron Type -> Html Msg
preview expands current root =
    let
        helper prop =
            let
                ( path, labelPath ) =
                    prop
                        |> Property.get
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ( Path.start, [] )
                isExpanded =
                    expands
                        |> Dict.get (Path.toList path)
                        |> Maybe.withDefault False
                previewProp =
                    viewCellAsALine
                        (Path.equal path current)
                        isExpanded
                        ( path, labelPath )
                        (prop |> Property.map Tuple.second)
                viewItems control =
                    if isExpanded then
                        Nest.getItems control
                            |> Array.map Tuple.second
                            |> Array.toList
                            |> List.map
                                (helper >> List.singleton >> Html.li [])
                    else []

            in
                case prop of
                    Property.Group _ _ control ->
                        Html.div
                            []
                            [ previewProp
                            , Html.ul [] <| viewItems control
                            ]
                    Property.Choice _ _ control ->
                        Html.div
                            []
                            [ previewProp
                            , Html.ul [] <| viewItems control
                            ]
                    _ ->
                        previewProp
    in
        helper <| Property.addPaths <| root


    {-
    Property.fold3 (\path cell before -> ( path, cell ) :: before) []
        >> List.reverse
        -- |> List.sortBy (Tuple.first >> Path.toList)
        >> List.map (\(path, cell) ->
            previewCell (Path.equal current <| Tuple.first path) path cell)
        >> Html.div [ Html.id "tree" ]
    -}


viewCellAsALine : Bool -> Bool -> ( Path, LabelPath ) -> Tron Type -> Html Msg
viewCellAsALine isCurrent isExpanded (path, labelPath) prop =
    Html.div
        [ Html.onClick <| SwitchTo (path, labelPath) prop
        , Html.class "edit-cell-line"
        , Html.class <| if isCurrent then "edit-cell-line--current" else ""
        ]
        [ if Path.howDeep path > 0
            then viewPath path
            else emptyPath
        , if List.length labelPath > 0
            then viewLabelPath labelPath
            else emptyLabelPath
        , Html.span
            [ Html.class "cell-type" ]
            [ Html.text <| typeToString <| typeOf prop ]
        , case typeOf prop of
            Choice -> expandCollapseButton path isExpanded
            Group -> expandCollapseButton path isExpanded
            _ -> Html.span [] []
        ]


expandCollapseButton : Path -> Bool -> Html Msg
expandCollapseButton path isExpanded =
    Html.button
        [ Html.class "expand-collapse"
        , Html.onClick <| ExpandOrCollapse path
        ]
        [ Html.text <| if isExpanded then "▲" else "▼" ]


previewCell : Bool -> ( Path, LabelPath ) -> Tron Type -> Html Msg
previewCell isCurrent (path, labelPath) prop =
    Html.div
        [ Html.onClick <| SwitchTo (path, labelPath) prop
        , Html.class "edit-cell"
        , Html.class <| if isCurrent then "edit-cell--current" else ""
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
    let
        maybeIdx = Path.pop path |> Maybe.map Tuple.second
    in

    Html.div
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
            [ Html.class "move-forward"
            , Html.onClick <| case maybeIdx of
                Just idx -> Forward idx
                Nothing -> NoOp
            ]
            [ Html.text "▶︎" ]
        , Html.span
            [ Html.class "move-backward"
            , Html.onClick <| case maybeIdx of
                Just idx -> Backward idx
                Nothing -> NoOp
            ]
            [ Html.text "◀︎" ]
        , Html.span
            [ Html.class "verb verb--danger"
            , Html.onClick <| case maybeIdx of
                Just idx -> Remove idx
                Nothing -> NoOp
            ]
            [ Html.text "✕" ]
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
            [ Html.text "root" ]
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
            prop |> edit_ (create >> fillTypes) D.string value
        ( Button, "icon" ) ->
            prop |> editIcon value
        ( Choice, "icon" ) ->
            prop |> editIcon value
        ( Group, "icon" ) ->
            prop |> editIcon value
        ( Button, "color" ) ->
            prop |> Property.setFace (Button.WithColor <| Color.yellow)
        ( Group, "color" ) ->
            prop |> Property.setFace (Button.WithColor <| Color.yellow)
        ( Choice, "color" ) ->
            prop |> Property.setFace (Button.WithColor <| Color.yellow)
        ( Button, "auto" ) ->
            prop |> Property.clearFace
        ( Group, "auto" ) ->
            prop |> Property.clearFace
        ( Choice, "auto" ) ->
            prop |> Property.clearFace
        _ -> prop


editIcon : E.Value -> Tron Type -> Tron Type
editIcon value prop =
    prop |>
        edit_
            (\iconPath ->
                if List.length iconPath > 0 then
                    prop
                        |> Tron.face (Tron.iconAt iconPath)
                else prop |> Property.setFace Button.Default
            )
            (D.list D.string)
            value


edit_ : (x -> Tron a) -> D.Decoder x -> E.Value -> Tron a -> Tron a
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
subscriptions _ =
    receiveFromLocalStorage (D.decodeValue Exp.decode)
        |> Sub.map (\result ->
            case result of
                Ok prop -> FromLocalStorage prop
                Err _ -> NoOp
        )


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
                let
                    nextModel = update msg model
                in
                ( nextModel
                , case msg of
                    LoadExample _ ->
                        updateCodeMirror ()
                    Save ->
                        updateCodeMirror ()
                    TriggerFromLocalStorage ->
                        triggerLoadFromLocalStorage ()
                    ToLocalStorage ->
                        sendToLocalStorage <| Exp.encode <| nextModel.tree
                    FromLocalStorage _ ->
                        updateCodeMirror ()
                    _ -> Cmd.none
                )
            )
        , subscriptions = subscriptions
        }


port updateCodeMirror : () -> Cmd msg


port sendToLocalStorage : Exp.Property -> Cmd msg


port triggerLoadFromLocalStorage : () -> Cmd msg


port receiveFromLocalStorage : (Exp.Property -> msg) -> Sub msg
