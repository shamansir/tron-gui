module Tron.Render.Control.Button exposing (..)


import Bounds exposing (BoundsF)
import Color as Color exposing (..)
import Url as Url

import Tron.Path as Path
import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Button as Button

import Tron.Style.Theme exposing (Theme)
import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Cell as Cell
import Tron.Style.CellShape as CS exposing (CellShape)
import Tron.Style.Selected exposing (Selected(..))

import Tron.Render.Context as Context exposing (Context)
import Tron.Render.Util exposing (resetTransform, arrow)
import Tron.Render.Util as Svg
import Tron.Render.Transform as T
import Tron.Render.Control.Color as Color

import Svg as Svg exposing (Svg)
import Svg.Attributes as SA


-- view : Theme -> State -> BoundsF -> Toggle.Control a -> Svg msg
view : Theme -> Context -> Button.Control a -> Path.Label -> Svg msg
view theme ctx (Control face _ _) label =
    viewFace theme ctx face label


viewFace : Theme -> Context -> Button.Face -> Path.Label -> Svg msg
viewFace theme ctx face label =
    let

        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        bounds = ctx.bounds
        ( labelX, labelY ) =
            if CS.isHorizontal ctx.cellShape
                then
                    case face of
                        Button.Empty -> ( 30, cy + 4 )
                        Button.Title -> ( 30, cy + 4 ) -- FIXME
                        Button.Expand -> ( 30, cy + 4 ) -- FIXME
                        Button.Focus -> ( 30, cy + 4 ) -- FIXME
                        Button.WithIcon _ -> ( 40, cy )
                        Button.WithColor _ -> ( 40, cy )
                else ( cx, cy )
        textLabel _ =
            Svg.text_
                [ SA.x <| String.fromFloat labelX
                , SA.y <| String.fromFloat labelY
                , SA.class "button__label"
                , SA.fill <| Color.toCssString <| Coloring.text theme <| Context.styleDef ctx
                , SA.mask <|
                    if not <| CS.isHorizontal ctx.cellShape
                        then "url(#button-text-mask)"
                        else "url(#button-text-mask-wide)"
                ]
                [ Svg.text label ]

    in case face of

        Button.Empty ->
            if CS.isHorizontal ctx.cellShape
                then case ctx.selected of
                    Selected ->
                        Svg.g
                            [ resetTransform ]
                            [ Svg.g
                                [ SA.style <|
                                    "transform: "
                                        ++ "translate(" ++ String.fromFloat Cell.gap ++ "px,"
                                                        ++ String.fromFloat (cy - 4) ++ "px)" ]
                                [ arrow (Coloring.text theme <| Context.styleDef ctx) (T.scale 0.5) (T.rotate 90)
                                ]
                            -- , textLabel ( bounds.width / 2.25 + gap, cy )
                            , textLabel ()
                            ]
                    Usual -> textLabel ()
                else textLabel ()

        Button.Title ->
            Svg.g [] [] -- FIXME

        Button.Focus ->
            Svg.g [] [] -- FIXME

        Button.Expand ->
            Svg.g [] [] -- FIXME

        Button.WithIcon (Button.Icon icon) ->
            let
                iconUrl =
                    icon theme |> Maybe.map Button.maybeLocalUrlToString |> Maybe.withDefault ""
                    --"./assets/" ++ icon ++ "_" ++ Theme.toString theme ++ ".svg"
                ( iconWidth, iconHeight ) = iconSize ctx.cellShape bounds
                ( iconX, iconY ) =
                    if CS.isHorizontal ctx.cellShape
                        then
                            ( -20, cy - iconHeight / 2 + 1 )
                        else
                            ( cx - iconWidth / 2, cy - iconHeight / 2 + 1 )
            in
                Svg.g
                    [ resetTransform ]
                    [
                        Svg.image
                        [ SA.xlinkHref <| iconUrl
                        , SA.class "button__icon"
                        , SA.width <| String.fromFloat iconWidth ++ "px"
                        , SA.height <| String.fromFloat iconHeight ++ "px"
                        , SA.x <| String.fromFloat iconX
                        , SA.y <| String.fromFloat iconY
                        ]
                        []
                    , if CS.isHorizontal ctx.cellShape
                        then textLabel ()
                        else Svg.none
                    ]

        Button.WithColor theColor ->
            case CS.units ctx.cellShape of
                ( CS.Single, CS.Single ) ->
                    Color.viewValue theme ctx theColor
                ( CS.Twice, _ ) ->
                    Svg.g
                        []
                        [ Color.viewValue
                            theme
                            { ctx
                            | bounds =
                                { bounds
                                | width = bounds.height
                                }
                            }
                            theColor
                        , textLabel ()
                        ]
                _ ->
                    let
                        ( rectWidth, rectHeight ) = ( bounds.width, bounds.height )
                        ( rectX, rectY ) = ( cx - rectWidth / 2, cy - rectHeight / 2 )
                    in
                        Svg.rect
                            [ SA.x <| String.fromFloat rectX
                            , SA.y <| String.fromFloat rectY
                            , SA.width <| String.fromFloat rectWidth
                            , SA.height <| String.fromFloat rectHeight
                            , SA.fill <| Color.toCssString theColor
                            , SA.rx "3"
                            , SA.ry "3"
                            ]
                            [
                            ]

iconSize : CellShape -> BoundsF -> ( Float, Float )
iconSize cs bounds =
    case CS.units cs of
        ( CS.Single, CS.Single ) -> ( 32, 32 )
        _ -> ( bounds.width / 2.25, bounds.height / 2.25 )