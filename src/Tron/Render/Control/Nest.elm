module Tron.Render.Control.Nest exposing (..)


import Array exposing (Array)
import Bounds exposing (BoundsF)
import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Nest as Nest
import Tron.Control.Impl.Button as Button
import Tron.Render.Control.Button as Button
import Tron.Tree exposing (Tree)

import Tron.Style.Theme exposing (Theme)
import Tron.Render.Context as Context exposing (Context, StyleDef)
import Tron.Render.Util as Util exposing (resetTransform, describeArc, describeMark)
import Tron.Render.Transform as T
import Tron.Style.Coloring as Coloring exposing (..)
-- import Tron.Style.Cell as Cell
import Tron.Path as Path

import Svg exposing (Svg)
import Svg.Attributes as SA



viewChoice
     : (( Path.Label, Tree a ) -> Svg msg)
    -> Theme
    -> Context
    -> Path.Label
    -> Nest.ChoiceControl ( Path.Label, item ) a
    -> Maybe ( Path.Label, Tree a )
    -> Svg msg
viewChoice viewPropAtPlace theme ctx label ( Control items { form, face, mode, selected } _) maybeSelectedInside =
    case ( mode, face, maybeSelectedInside ) of

        -- TODO: should be represented using button renderer
        ( _, Just Button.Focus, Just theSelectedProp ) ->
            viewPropAtPlace
                theSelectedProp

        -- TODO: should be represented using button renderer
        ( _, Just Button.Expand, _ ) ->
            arrow theme (Context.styleDef ctx) form ctx.bounds

        ( _, Just buttonFace, _ ) ->
            Button.viewFace theme ctx buttonFace label

        ( Nest.Pages, Nothing, Just theSelectedProp ) ->
            viewPropAtPlace
                theSelectedProp

        ( Nest.SwitchThrough, Nothing, Just theSelectedProp ) ->
            viewPropAtPlace
                theSelectedProp

        ( Nest.Knob, Nothing, _ ) ->
            knobSwitch
                theme
                (Context.styleDef ctx)
                ctx.bounds
                (items |> Array.map Tuple.first)
                selected

        ( Nest.SwitchThrough, Nothing, Nothing ) ->
            arrow theme (Context.styleDef ctx) form ctx.bounds

        ( Nest.Pages, Nothing, Nothing ) ->
            arrow theme (Context.styleDef ctx) form ctx.bounds


viewGroup : Theme -> Context -> Path.Label -> Nest.GroupControl items a -> Svg msg
viewGroup theme ctx label ( Control _ { form, face } _) =

    case face of
        Just Button.Expand ->
            arrow theme (Context.styleDef ctx) form ctx.bounds

        Just buttonFace ->
            Button.viewFace theme ctx buttonFace label

        Nothing -> arrow theme (Context.styleDef ctx) form ctx.bounds



knobSwitch : Theme -> StyleDef -> BoundsF -> Array String -> Int -> Svg msg
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



arrow : Theme -> StyleDef -> Nest.Form -> BoundsF -> Svg msg
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
        [ Util.arrow (Coloring.lines theme state) (T.scale scaleV)
            <| case form of
                Nest.Expanded -> T.rotate 180
                Nest.Detached -> T.rotate 45
                Nest.Collapsed -> T.rotate 0
        ]
