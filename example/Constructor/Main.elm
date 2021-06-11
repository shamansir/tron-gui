module Constructor.Main exposing (..)


import Browser exposing (Document)

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
    ( Maybe (Tron Def)
    , Tron ()
    )


type alias Def = ( ( Path, LabelPath ), Type )


type Msg
    = NoOp
    | Save
    | Append
    | Edit (Tron Def)


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
            , case current |> Maybe.andThen extractPath of
                Just ( newProp, path ) ->
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
                        (Property.append
                            ( "test"
                            , Tron.none
                                |> fillTypes -- FIXME: paths are wrong this way
                            )
                        )
            , currentGui
            )
        Edit prop ->
            ( Just prop
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
            Just currentProp ->
                editorFor currentProp
            Nothing -> Html.div [] []
        ]


fillTypes : Tron () -> Tron Def
fillTypes =
    Property.reflect
        >> Property.map Tuple.first
        >> Property.map valueToType
        >> Property.addPaths


extractPath : Tron Def -> Maybe ( Tron Def, Path )
extractPath prop =
    Property.get prop
        |> Maybe.map
            (Tuple.first >> Tuple.first >> Tuple.pair prop)


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


editorFor : Tron Def -> Html Msg
editorFor prop =
    Html.div
        []
        [ case Property.get prop
                |> Maybe.map Tuple.second of
            Just Group ->
                Html.button
                    [ Html.onClick <| Append ]
                    [ Html.text "Append" ]
            Just _ -> Html.div [] []
            Nothing -> Html.div [] []
        , Html.button
            [ Html.onClick <| Save ]
            [ Html.text "Save" ]
        ]


preview : Tron Def -> Html Msg
preview tree =
    tree
        |> Property.fold
            (\path cell before ->
                previewCell 0 cell :: before
            )
            []
        |> Html.div []


previewCell : Int -> Tron Def -> Html Msg
previewCell _ prop =
    case prop |> Property.get of
        Just ( (path, labelPath), type_ ) ->
            Html.button
                [ Html.onClick <| Edit prop ]
                [ Html.text <| "Edit (" ++ typeToString type_ ++ ")" ]
        Nothing ->
            Html.button
                [ Html.onClick <| Edit prop ]
                [ Html.text "+" ]


addGhosts : Tron () -> Tron ()
addGhosts =
    Property.replace
        <| always
        -- it will skip all non-group elements
        <| Property.append ( "_Add", Tron.none )


-- subscriptions : Model -> Sub Msg
-- subscriptions _ = Sub.none


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