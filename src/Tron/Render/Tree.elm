module Tron.Render.Tree exposing (..)


import Bounds exposing (..)
import Color as Color exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Events as HE

import Tron.Tree.Internals exposing (..)
import Tron.Tree.Controls exposing (..)
import Tron.Path as Path exposing (Path)
import Tron.Msg exposing (Msg_(..))
import Tron.Focus exposing (Focused(..))

import Tron.Control.Impl.Text exposing (TextState(..))
import Tron.Control.Impl.Button exposing (Face(..), Icon(..))
import Tron.Control.Impl.Toggle exposing (ToggleState(..))
import Tron.Control.Impl.Nest as Nest exposing (Form(..))

import Tron.Render.Transform exposing (..)
import Tron.Render.Util exposing (..)
import Tron.Render.Util as Svg
import Tron.Render.Context as Context exposing (Context)

import Tron.Render.Control.Number as Number
import Tron.Render.Control.XY as XY
import Tron.Render.Control.Text as Text
import Tron.Render.Control.Toggle as Toggle
import Tron.Render.Control.Color as Color
import Tron.Render.Control.Button as Button
import Tron.Render.Control.Nest as Nest

import Tron.Style.Logic exposing (..)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS exposing (..)

import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Theme exposing (Theme(..))
import Tron.Style.Selected exposing (Selected(..))
import Tron.Style.Cell as Cell


view
     : Theme
    -> Context
    -> Path
    -> Maybe ( Path.Label, Tree a )
    -> ( Path.Label, Tree a )
    -> Svg Msg_
view theme ctx path maybeSelectedInside ( label, prop ) =
    Svg.g
        [ HE.onClick <| Click path
        , SA.class <| makeClass ctx.cellShape <| prop
        ]
        [ Svg.rect
            [ SA.fill
                <| Color.toCssString
                <| Coloring.back theme <| Context.styleDef ctx
            , SA.x <| String.fromFloat (Cell.gap / 2)
            , SA.y <| String.fromFloat (Cell.gap / 2)
            , SA.rx <| String.fromFloat Cell.borderRadius
            , SA.ry <| String.fromFloat Cell.borderRadius
            , SA.width <| String.fromFloat (ctx.bounds.width - Cell.gap) ++ "px"
            , SA.height <| String.fromFloat (ctx.bounds.height - Cell.gap) ++ "px"
            , SA.strokeWidth
                <| String.fromInt (strokeWidthFor ctx <| isExpanded prop) ++ "px"
            , SA.stroke
                <| Color.toCssString
                <| Coloring.border theme (Context.styleDef ctx) <| isExpanded prop
            --, SA.strokeDasharray <| strokeDashFor state
            ]
            []
        , viewTree
            theme ctx path maybeSelectedInside ( label, prop )
        , {- case prop of
            Action _ -> Svg.none
            _ -> -} viewLabel theme ctx label
        ]


viewLabel
    :  Theme
    -> Context
    -> Path.Label
    -> Svg msg
viewLabel theme ctx label =
    if CS.isSquare ctx.cellShape
        then
            Svg.text_
                [ SA.class "cell__label"
                , SA.x <| String.fromFloat (ctx.bounds.width / 2)
                , SA.y <| String.fromFloat (ctx.bounds.height / 5 * 4)
                , SA.fill <| Color.toCssString <| Coloring.text theme <| Context.styleDef ctx
                ]
                [ Svg.text label ]
        else Svg.none


viewTree
     : Theme
    -> Context
    -> Path
    -> Maybe ( Path.Label, Tree a )
    -> ( Path.Label, Tree a )
    -> Svg Msg_
viewTree
    theme
    ctx
    path
    maybeSelectedInside
    ( label, prop ) =
    case prop of

        Number control ->

            Number.view theme ctx control

        Coordinate control ->

            XY.view theme ctx control

        Text control ->

            Text.view theme ctx control
                |> Svg.map (TextInput path)

        Toggle control ->

            Toggle.view theme ctx control

        Action control ->

            Button.view theme ctx control label

        {- Switch (Control items ( _, value ) _) ->

            switch theme state bounds items value -}

        Color control ->

            Color.view theme ctx control

        Choice _ _ control ->

            Nest.viewChoice
                (viewTree
                    theme
                    { ctx | selected = Selected }
                    path
                    Nothing
                ) theme ctx label control maybeSelectedInside

        Group _ _ control ->

            Nest.viewGroup theme ctx label control

        Live innerProp ->
            viewTree
                theme
                ctx
                path
                maybeSelectedInside
                ( label, innerProp )

        _ -> Svg.none


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





strokeWidthFor : Context -> Maybe Nest.Form -> Int
strokeWidthFor { focused, selected } maybeCollapsed =
    case maybeCollapsed of
        Just Nest.Expanded -> 1
        _ ->
            case selected of
                Selected -> 1
                Usual ->
                    case focused of
                        FocusedBy _  -> 1
                        _ -> 0


strokeDashFor : Context -> String
strokeDashFor { focused } =
    case focused of
        FocusedBy _ -> "5,1"
        NotFocused -> ""