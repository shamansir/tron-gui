port module Constructor.Main exposing (..)


import Browser exposing (Document)
import Array
import Json.Decode as D
import Json.Encode as E
import Color
import Dict exposing (Dict)

import Tron exposing (Tron)
import Tron.Tree.Build.Unit as Build
import Tron.Option.Render as Render
import Tron.Option.Communication as Communication
import Tron.Style.Dock as Dock
import Tron.Style.Theme as Theme
import Tron.Path as Path exposing (Path)

import Tron.Core as Core
import Tron.Tree as Tree exposing (Tree)
import Tron.Tree.Paths as Tree
import Tron.Tree.Controls as Tree
import Tron.Path as Path
import Tron.Control as Core
import Tron.Control.Impl.Nest as Nest
import Tron.Control.Value as V exposing (Value)
import Tron.Control.Impl.Button as Button
import Tron.Tree.Expose as Exp
import Tron.Tree.Expose.Data as Exp

import WithTron.ValueAt as V
import Tron.Style.PanelShape as PS
import Tron.Style.CellShape as CS

import Size
import WithTron

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
    { current : Maybe ( Path, Tree Type )
    , expands : Dict (List Path.Index) Bool
    , tree : Tree ()
    }


type Msg
    = NoOp
    | Save
    | Append
    | Forward Int
    | Backward Int
    | Remove Int
    | SwitchTo Path (Tree Type)
    | Edit String E.Value
    | EditLabel String
    | EditPanelShape PS.PanelShape
    | EditCellShape CS.CellShape
    | EditChoiceMode Nest.ChoiceMode
    | LoadExample Example
    | ToLocalStorage
    | TriggerFromLocalStorage
    | FromLocalStorage (Tree ())
    | ExpandOrCollapse Path
    | TogglePagination


for : Model -> Tron Msg
for =
    .tree
    >> Tree.map (always NoOp)
    >> Tron.lift


init : Model
init =
    { current = Nothing
    , tree =
        Build.root
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
            , tree =
                case model.current of
                    Just ( path, newProp ) ->
                        model.tree
                            |> Tree.foldP
                                (\otherPath otherProp ->
                                    if Path.toList otherPath == Path.toList path then
                                        newProp |> Tree.toUnit
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
                        <| Tree.append
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
                        (Tuple.mapSecond <| Tree.remove idx)
            }

        Forward idx ->
            { model
            | current =
                model.current
                    |> Maybe.map
                        (Tuple.mapSecond <| Tree.forward idx)
            }

        Backward idx ->
            { model
            | current =
                model.current
                    |> Maybe.map
                        (Tuple.mapSecond <| Tree.backward idx)
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
                |> Maybe.map (Tuple.mapSecond <| Tree.setCellShape newCellShape)
            }

        EditPanelShape newPanelShape ->
            { model
            | current =
                model.current
                |> Maybe.map (Tuple.mapSecond <| Tree.setPanelShape newPanelShape)
            }

        TogglePagination ->
            { model
            | current =
                model.current
                |> Maybe.map (Tuple.mapSecond <| Tree.togglePagination)
            }

        EditChoiceMode newChoiceMode ->
            { model
            | current =
                model.current
                    |> Maybe.map (Tuple.mapSecond <| Tree.setChoiceMode newChoiceMode)
            }

        EditLabel newLabel ->
            case model.current of
                Just ( path, prop ) ->
                    { model
                    | current =
                        Just
                            ( ( path |> changeLastTo newLabel ), prop )
                    , tree =
                        model.tree |> Tree.changeLabel path newLabel
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
                Empty -> Build.root []
                Default -> Example_Default.for Example_Default.default |> Tree.toUnit
                Goose -> Example_Goose.for Example_Goose.default |> Tree.toUnit
                Tiler -> Example_Tiler.gui Tree.empty (Example_Tiler.init () |> Tuple.first) |> Tree.toUnit
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
                        (path |> Path.toIndexPath)
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
                    |> Maybe.map Tuple.first
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


fillTypes : Tree a -> Tree Type
fillTypes =
    Exp.reflect
        >> Tree.map valueToType


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
        V.None -> None


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


editorFor : Path -> Tree Type -> Html Msg
editorFor path prop =
    let

        shapeEditor ( panelShape, cellShape ) =
            Html.div
                [Html.class "shape"]
                [ viewPanelShapeSelector panelShape EditPanelShape
                , viewCellShapeSelector cellShape EditCellShape
                ]

        itemsEditor items =
            items
                |> Array.indexedMap
                    (\idx (label, prop_) ->
                        previewNestCell
                        -- editorFor
                            ( path |> Path.advance ( idx, label ) )
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

        paginationSwitch ps =
            Html.div
                []
                [ Html.input
                    [ Html.type_ "checkbox"
                    , Html.onClick <| TogglePagination
                    , Html.checked <| PS.pagesEnabled ps
                    , Html.id "pagination-cb"
                    ]
                    []
                , Html.label
                    [ Html.for "pagination-cb" ]
                    [ Html.text "Pagination" ]
                ]

    in
    Html.div
        [ Html.class "editor" ]

        [ viewPath path
        , viewLabelPath path

        , case path |> Path.first |> Maybe.map Tuple.second of
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
            Tree.Action (Core.Control face _ _) ->
                faceEditor <| Just face

            Tree.Group _ shape (Core.Control _ { face } _ as control) ->
                Html.div
                    []
                    [ faceEditor <| face
                    , itemsEditor <| Nest.getItems control
                    , Html.button
                        [ Html.onClick <| Append ]
                        [ Html.text "Append" ]
                    , paginationSwitch <| Tuple.first shape
                    , shapeEditor shape
                    ]

            Tree.Choice _ shape (Core.Control _ { face } _ as control) ->
                Html.div
                    []
                    [ faceEditor <| face
                    , choiceModeEditor
                    , itemsEditor <| Nest.getItems control
                    , Html.button
                        [ Html.onClick <| Append ]
                        [ Html.text "Append" ]
                    , paginationSwitch <| Tuple.first shape
                    , shapeEditor shape
                    ]

            _ -> Html.div [] []

        , Html.button
            [ Html.onClick <| Save ]
            [ Html.text "Save" ]
        ]


typeOf : Tree Type -> Type
typeOf = Tree.get


preview : Dict (List Path.Index) Bool -> Path -> Tree Type -> Html Msg
preview expands current root =
    let
        helper prop =
            let
                path =
                    prop |> Tree.get |> Tuple.first
                isExpanded =
                    expands
                        |> Dict.get (Path.toIndexPath path)
                        |> Maybe.withDefault False
                previewProp =
                    viewCellAsALine
                        (Path.equal path current)
                        isExpanded
                        path
                        (prop |> Tree.map Tuple.second)
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
                    Tree.Group _ _ control ->
                        Html.div
                            []
                            [ previewProp
                            , Html.ul [] <| viewItems control
                            ]
                    Tree.Choice _ _ control ->
                        Html.div
                            []
                            [ previewProp
                            , Html.ul [] <| viewItems control
                            ]
                    _ ->
                        previewProp
    in
        helper
            <| Tree.squeezeMap2
                Tuple.pair
                (Tree.pathify root)
                (fillTypes root)


    {-
    Tree.fold3 (\path cell before -> ( path, cell ) :: before) []
        >> List.reverse
        -- |> List.sortBy (Tuple.first >> Path.toList)
        >> List.map (\(path, cell) ->
            previewCell (Path.equal current <| Tuple.first path) path cell)
        >> Html.div [ Html.id "tree" ]
    -}


viewCellAsALine : Bool -> Bool -> Path -> Tree Type -> Html Msg
viewCellAsALine isCurrent isExpanded path prop =
    Html.div
        [ Html.onClick <| SwitchTo path prop
        , Html.class "edit-cell-line"
        , Html.class <| if isCurrent then "edit-cell-line--current" else ""
        ]
        [ if Path.howDeep path > 0
            then viewPath path
            else emptyPath
        , if Path.length path > 0
            then viewLabel path
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


previewCell : Bool -> Path -> Tree Type -> Html Msg
previewCell isCurrent path prop =
    Html.div
        [ Html.onClick <| SwitchTo path prop
        , Html.class "edit-cell"
        , Html.class <| if isCurrent then "edit-cell--current" else ""
        ]
        [ if Path.howDeep path > 0
            then viewPath path
            else emptyPath
        , if Path.length path > 0
            then viewLabelPath path
            else emptyLabelPath
        , Html.span [ Html.class "cell-type" ]
            [ Html.text <| typeToString <| typeOf prop ]
        , Html.span
            [ Html.class "verb" ] [ Html.text "Edit" ]
        ]


previewNestCell : Path -> Tree Type -> Html Msg
previewNestCell path prop =
    let
        maybeIdx =
            Path.last path
                |> Maybe.map Tuple.first
    in

    Html.div
        [ Html.class "edit-cell"
        , Html.class "edit-cell--preview"
        ]
        [ if Path.howDeep path > 0
            then viewPath path
            else emptyPath
        , if Path.length path > 0
            then viewLabelPath path
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
        >> List.map (Tuple.first >> String.fromInt)
        >> List.map (\n -> Html.span [ Html.class "item" ] [ Html.text n ])
        >> List.intersperse (Html.span [ Html.class "sep" ] [ Html.text "/" ])
        >> Html.span [ Html.class "path" ]


viewLabelPath : Path -> Html msg
viewLabelPath =
    Path.toList
        >> List.map Tuple.second
        >> List.map (\n -> Html.span [ Html.class "item" ] [ Html.text n ])
        >> List.intersperse (Html.span [ Html.class "sep" ] [ Html.text "/" ])
        >> Html.span [ Html.class "label-path" ]


viewLabel : Path -> Html msg
viewLabel path =
    Html.span
        [ Html.class "label-path" ]
        [ case Path.last path |> Maybe.map Tuple.second of
            Just last -> Html.span [ Html.class "item" ] [ Html.text last ]
            Nothing -> Html.span [] []
        ]


changeLastTo : String -> Path -> Path
changeLastTo newLabel path =
    let
        reversed = List.reverse <| Path.toList path
    in
        (case reversed |> List.head of
            Just (prevIndex, _) ->
                (prevIndex, newLabel) :: (List.tail reversed |> Maybe.withDefault [])
            Nothing ->
                []
        ) |> List.reverse |> Path.fromList


edit : String -> E.Value -> Tree Type -> Tree Type
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
            prop |> Tree.setFace (Button.WithColor <| Color.yellow)
        ( Group, "color" ) ->
            prop |> Tree.setFace (Button.WithColor <| Color.yellow)
        ( Choice, "color" ) ->
            prop |> Tree.setFace (Button.WithColor <| Color.yellow)
        ( Button, "auto" ) ->
            prop |> Tree.clearFace
        ( Group, "auto" ) ->
            prop |> Tree.clearFace
        ( Choice, "auto" ) ->
            prop |> Tree.clearFace
        _ -> prop


editIcon : E.Value -> Tree Type -> Tree Type
editIcon value prop =
    prop |>
        edit_
            (\iconPath ->
                if List.length iconPath > 0 then
                    prop
                        |> Build.face (Build.iconAt iconPath)
                else prop |> Tree.setFace Button.Default
            )
            (D.list D.string)
            value


edit_ : (x -> Tree a) -> D.Decoder x -> E.Value -> Tree a -> Tree a
edit_ f decoder value prop =
    case value |> D.decodeValue decoder of
        Ok decodedValue ->
            f decodedValue
        Err _ -> prop


create : String -> Tree ()
create s =
    case s of
        "Knob" ->
            Build.float { min = 0, max = 1, step = 0.01 } 0
        "XY" ->
            Build.xy
                ( { min = 0, max = 1, step = 0.01 }
                , { min = 0, max = 1, step = 0.01 }
                )
                ( 0, 0 )
        "Text" ->
            Build.text "foo"
        "Toggle" ->
            Build.toggle False
        "Color" ->
            Build.color Color.black
        "Choice" ->
            Build.choice
                [
                ]
                "foo"
        "Button" ->
            Build.button
        "Nest" ->
            Build.nest []
        _ -> Build.none


viewCode : Tree () -> Html msg
viewCode =
    ToBuilder.toCodeLines
        >> String.join "\n"
        >> Html.text
        >> List.singleton
        >> Html.textarea [ Html.id "builder-code" ]


{- addGhosts : Tree () -> Tree ()
addGhosts =
    Tree.mapWithPath
        <| always
        -- it will skip all non-group elements
        <| Tree.append ( "_Add", Build.none ) -}


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


main : WithTron.Program () Model Msg
main =
    WithTron.element
        (Render.toHtml Dock.bottomCenter Theme.dark)
        Communication.none
        { for = \_ -> for
        , init = \_ -> ( init, Cmd.none )
        , view = \_ -> view
        , update =
            (\msg _ model ->
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
        , subscriptions = \_ -> subscriptions
        }


port updateCodeMirror : () -> Cmd msg


port sendToLocalStorage : Exp.Tree -> Cmd msg


port triggerLoadFromLocalStorage : () -> Cmd msg


port receiveFromLocalStorage : (Exp.Tree -> msg) -> Sub msg
