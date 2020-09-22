module Gui.Render.Layout exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input, br)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Html.Events as H
import Json.Decode as Json
import Dict

import BinPack
import Bounds exposing (..)
import Bounds as B exposing (..)

import Gui.Path as Path exposing (Path)
import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (find)
import Gui.Msg exposing (..)
import Gui.Layout exposing (..)
import Gui.Focus exposing (Focused(..), focused)

import Gui.Render.Util exposing (..)
import Gui.Render.Debug exposing (..)
import Gui.Render.Property as Property exposing (..)
import Gui.Render.Plate as Plate exposing (..)
import Gui.Render.Style as Style exposing (..)


type alias GridView = Html Msg


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


mode : Mode
mode = Fancy


viewProperty
    :  Style.Theme
    -> Tone
    -> Path
    -> Bounds
    -> Focused
    -> ( Label, Property msg )
    -> Svg Msg
viewProperty theme tone path pixelBounds focus ( label, prop ) =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g
                    [ SA.class "cell--debug"
                    , H.onClick <| Click path
                    ]
                    <| List.map (Svg.map <| always NoOp)
                    <| [ rect_ "white" pixelBounds
                    , boundsDebug pixelBounds -- FIXME: were in cells before, not in pixels
                    , textAt 5 20
                        <| case focus of
                            FocusedBy level -> "focused " ++ String.fromInt level
                            NotFocused -> ""
                    , positionAt 0 30
                        <| propertyDebug ( label, prop )
                    ]
            Fancy ->
                Property.view theme tone path pixelBounds focus ( label, prop )


viewPlateBack : Style.Theme -> Bounds -> Svg Msg
viewPlateBack theme pixelBounds =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ SA.class "plate--debug" ]
                    [ rect_ "beige" pixelBounds
                    , boundsDebug pixelBounds
                    ]
            Fancy ->
                Plate.back theme pixelBounds


viewPlateControls : Tone -> Bounds -> Path -> Svg Msg
viewPlateControls tone pixelBounds path =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Plate.controls tone pixelBounds path


view : Style.Theme -> Bounds -> Property msg -> Layout -> Html Msg
view theme bounds root layout =
    let
        keyDownHandler_ =
            H.on "keydown"
                <| Json.map KeyDown H.keyCode
        tones = Style.assignTones root
        toneOf path =
            tones |> Dict.get (Path.toString path) |> Maybe.withDefault Black
        ( plates, cells ) =
            BinPack.unfold
                (\( cell, cellBounds ) ( prevPlates, prevCells ) ->
                    case cell of

                        One path ->
                            ( prevPlates
                            , ( path, cellBounds ) :: prevCells
                            )

                        Plate originPath plateLayout ->
                            ( ( originPath, cellBounds ) :: prevPlates
                            , BinPack.unfold
                                (\( path, propBounds ) pPrevCells ->
                                    ( path, B.shift cellBounds propBounds ) :: pPrevCells
                                )
                                prevCells
                                plateLayout
                            )

                )
                ( [], [] )
                layout
        ( plates1, cells1 ) =
            ( plates |> List.map (Tuple.mapSecond <| B.multiplyBy cellWidth)
            , cells |> List.map (Tuple.mapSecond <| B.multiplyBy cellWidth)
            )
        ( platesBacksRendered, cellsRendered, platesControlsRendered ) =

            ( plates1 |> List.map (Tuple.second >> viewPlateBack theme)

            , cells1 |> List.map
                (\(path, propertyBounds) ->
                    case root |> Property.find1 path of
                        Just prop -> Just <|
                            viewProperty
                                theme
                                (toneOf path)
                                path
                                propertyBounds
                                (focused root path)
                                prop
                        Nothing -> Nothing
                )
                |> List.filterMap identity

            , plates1 |> List.map
                (\(path, plateBounds) ->
                    viewPlateControls (toneOf path) plateBounds path
                )
            )
        makeClass =
            "gui noselect "
                ++ (case mode of
                    Debug -> "gui--debug "
                    _ -> "")
                ++ (case theme of
                    Dark -> "--dark"
                    Light -> "--light")
    in
        div [ HA.id rootId
            , HA.class makeClass
            , HA.tabindex 0
            , keyDownHandler_
            ]
            [ Svg.svg
                [ SA.width <| String.fromFloat bounds.width ++ "px"
                , SA.height <| String.fromFloat bounds.height ++ "px"
                , SA.style <| "transform: translate("
                    ++ String.fromFloat bounds.x ++ "px,"
                    ++ String.fromFloat bounds.y ++ "px)"
                , SA.class "grid"
                ]
                [ Svg.g
                    []
                    [ Svg.g [] platesBacksRendered
                    , Svg.g [] cellsRendered
                    , Svg.g [] platesControlsRendered
                    ]
                ]

            ]

