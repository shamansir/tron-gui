module Tron.Render.Tree exposing (..)


import Bounds exposing (..)

import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Html
import Html.Attributes as HA
import Html.Events as HE
import Url

import Axis exposing (Axis)

import Tron.Tree.Internals exposing (..)
import Tron.Tree.Controls exposing (..)
import Tron.Path as Path exposing (Path)
import Tron.Msg exposing (Msg_(..))
import Tron.Focus exposing (Focused(..))
import Tron.Control as Core exposing (Control(..))

import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Button exposing (Face(..), Icon(..))
import Tron.Control.Impl.Toggle exposing (ToggleState(..))
import Tron.Control.Impl.Nest as Nest exposing (getForm, Form(..))
import Tron.Control.Impl.Number as Number exposing (Control)

import Tron.Render.Transform exposing (..)
import Tron.Render.Util exposing (..)
import Tron.Render.Util as Svg exposing (none)
import Tron.Render.Util as Util exposing (arrow)

import Tron.Render.Control.Number as Number
import Tron.Render.Control.XY as XY
import Tron.Render.Control.Text as Text
import Tron.Render.Control.Toggle as Toggle
import Tron.Render.Control.Color as Color
import Tron.Render.Control.Button as Button

import Tron.Style.Logic exposing (..)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS exposing (..)

import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Theme exposing (Theme(..))
import Tron.Style.Theme as Theme exposing (toString)
import Tron.Style.Placement exposing (Placement)
import Tron.Style.Selected exposing (Selected(..))
import Tron.Style.Cell as Cell

import Color as Color exposing (..)
import Url


view
     : Theme
    -> State
    -> Path
    -> BoundsF
    -> Maybe ( Path.Label, Tree a )
    -> CellShape
    -> ( Path.Label, Tree a )
    -> Svg Msg_
view theme state path bounds maybeSelectedInside cellShape ( label, prop ) =
    Svg.g
        [ HE.onClick <| Click path
        , SA.class <| makeClass cellShape <| prop
        ]
        [ Svg.rect
            [ SA.fill
                <| Color.toCssString
                <| Coloring.back theme state
            , SA.x <| String.fromFloat (Cell.gap / 2)
            , SA.y <| String.fromFloat (Cell.gap / 2)
            , SA.rx <| String.fromFloat Cell.borderRadius
            , SA.ry <| String.fromFloat Cell.borderRadius
            , SA.width <| String.fromFloat (bounds.width - Cell.gap) ++ "px"
            , SA.height <| String.fromFloat (bounds.height - Cell.gap) ++ "px"
            , SA.strokeWidth
                <| String.fromInt (strokeWidthFor state <| isExpanded prop) ++ "px"
            , SA.stroke
                <| Color.toCssString
                <| Coloring.border theme state <| isExpanded prop
            --, SA.strokeDasharray <| strokeDashFor state
            ]
            []
        , viewTree
            theme state path bounds maybeSelectedInside cellShape ( label, prop )
        , {- case prop of
            Action _ -> Svg.none
            _ -> -} viewLabel theme state cellShape bounds label
        ]


viewLabel
    :  Theme
    -> State
    -> CellShape
    -> BoundsF
    -> Path.Label
    -> Svg msg
viewLabel theme state cellShape bounds label =
    if CS.isSquare cellShape
        then
            Svg.text_
                [ SA.class "cell__label"
                , SA.x <| String.fromFloat (bounds.width / 2)
                , SA.y <| String.fromFloat (bounds.height / 5 * 4)
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                ]
                [ Svg.text label ]
        else Svg.none


viewTree
     : Theme
    -> State
    -> Path
    -> BoundsF
    -> Maybe ( Path.Label, Tree a )
    -> CellShape
    -> ( Path.Label, Tree a )
    -> Svg Msg_
viewTree
    theme
    ( ( placement, focus, _ ) as state )
    path
    bounds
    maybeSelectedInside
    cellShape
    ( label, prop ) =
    case prop of

        Number control ->

            Number.view theme state bounds control

        Coordinate control ->

            XY.view theme state bounds control

        Text control ->

            Text.view theme state bounds control
                |> Svg.map (TextInput path)

        Toggle control ->

            Toggle.view theme state bounds control

        Action (Control face _ _) ->

            Button.view theme state face cellShape label bounds

        {- Switch (Control items ( _, value ) _) ->

            switch theme state bounds items value -}

        Color control ->

            Color.view theme state bounds control

        Choice _ _ ( Control items { form, face, mode, selected } _) ->

            case ( mode, face, maybeSelectedInside ) of

                ( _, Just buttonFace, _ ) ->
                    Button.view theme state buttonFace cellShape label bounds

                ( Nest.Pages, Nothing, Just theSelectedProp ) ->
                    viewTree
                        theme
                        ( placement, focus, Selected )
                        path
                        bounds
                        Nothing
                        cellShape
                        theSelectedProp

                ( Nest.SwitchThrough, Nothing, Just theSelectedProp ) ->
                    viewTree
                        theme
                        ( placement, focus, Selected )
                        path
                        bounds
                        Nothing
                        cellShape
                        theSelectedProp

                ( Nest.Knob, Nothing, _ ) ->
                    knobSwitch
                        theme
                        state
                        bounds
                        (items |> Array.map Tuple.first)
                        selected

                ( Nest.SwitchThrough, Nothing, Nothing ) ->
                    arrow theme state form bounds

                ( Nest.Pages, Nothing, Nothing ) ->
                    arrow theme state form bounds

        Group _ _ ( Control _ { form, face } _) ->

            case face of
                Just buttonFace ->

                    Button.view theme state buttonFace cellShape label bounds

                Nothing -> arrow theme state form bounds

        Live innerProp ->
            viewTree
                theme
                state
                path
                bounds
                maybeSelectedInside
                cellShape
                ( label, innerProp )

        _ -> Svg.none


knobSwitch : Theme -> State -> BoundsF -> Array String -> Int -> Svg msg
knobSwitch theme state bounds items curItemId =
    let
        toAngle v = (-120) + (v * 120 * 2)
        path stroke d =
            Svg.path
                [ SA.d d
                , SA.fill "none"
                , SA.stroke stroke
                , SA.strokeWidth "2"
                , SA.strokeLinecap "round"
                ]
                []
        radiusA = (bounds.width * 0.27) - 1
        radiusB = bounds.height * 0.27
        ( cx, cy ) = ( bounds.width / 2, bounds.height / 2 )
        curItem = items |> Array.get curItemId |> Maybe.withDefault "?"
        relValue = Basics.toFloat curItemId / Basics.toFloat (Array.length items)
    in
        Svg.g
            [ resetTransform ]
            [ path (Coloring.lines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle 0, to = toAngle relValue }
            , path (Coloring.secondaryLines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle relValue, to = toAngle 1 }
            , path (Coloring.lines theme state |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    (toAngle relValue)
            , Svg.text_
                [ SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat cy
                , SA.class "knob__value"
                , SA.style "pointer-events: none"
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                ]
                [ Svg.text curItem ]
            ]


arrow : Theme -> State -> Form -> BoundsF -> Svg msg
arrow theme state form bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        scaleV = (min bounds.width bounds.height) / 127
    in Svg.g
        [ SA.style <|
            "transform: "
                ++ "translate(" ++ String.fromFloat (center.x - (14 * scaleV)) ++ "px,"
                                ++ String.fromFloat (center.y - (14 * scaleV)) ++ "px)"
        ]
        [ Util.arrow (Coloring.lines theme state) (scale scaleV)
            <| case form of
                Expanded -> rotate 180
                Detached -> rotate 45
                Collapsed -> rotate 0
        ]


makeClass : CellShape -> Tree a -> String
makeClass shape prop =
    let
        propTypeId prop_ =
            case prop_ of
                Nil _ -> "ghost"
                Number _ -> "number"
                Coordinate _ -> "coord"
                Text _ -> "text"
                Color _ -> "color"
                Toggle _ -> "toggle"
                Action _ -> "button"
                Choice _ _ _ -> "choice"
                Group _ _ _ -> "group"
                Live innerProp -> propTypeId innerProp
    in
    "cell"
        ++ " cell--" ++ propTypeId prop
        ++ " cell--" ++ shapeToModifier shape





strokeWidthFor : State -> Maybe Nest.Form -> Int
strokeWidthFor ( placement, focused, selected ) maybeCollapsed =
    case maybeCollapsed of
        Just Nest.Expanded -> 1
        _ ->
            case selected of
                Selected -> 1
                Usual ->
                    case focused of
                        FocusedBy _  -> 1
                        _ -> 0


strokeDashFor : State -> String
strokeDashFor ( _, focused , _ ) =
    case focused of
        FocusedBy _ -> "5,1"
        NotFocused -> ""