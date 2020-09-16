module Gui.Render.Layout exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input, br)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Html.Events as H
import Json.Decode as Json

import BinPack
import Bounds exposing (..)
import Bounds as B exposing (..)

import Gui.Path exposing (Path)
import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (find)
import Gui.Msg exposing (..)
import Gui.Layout exposing (..)
import Gui.Focus exposing (Focused(..), focused)

import Gui.Render.Util exposing (..)
import Gui.Render.Debug exposing (..)
import Gui.Render.Property as Property exposing (..)


type alias GridView = Html Msg


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


mode : Mode
mode = Debug


viewProperty : Property msg -> Path -> Bounds -> ( Label, Property msg ) -> Html Msg
viewProperty root path propBounds ( label, prop ) = -- FIXME: get rid of passing root
    positionAt_ (B.multiplyBy cellWidth <| propBounds) <|
        case mode of
            Debug ->
                S.g
                    [ SA.class "cell--debug"
                    , H.onClick <| Click path
                    ]
                    <| List.map (Svg.map <| always NoOp)
                    <| [ rect_ "white"
                        <| B.multiplyBy cellWidth
                        <| propBounds
                    , boundsDebug propBounds
                    , textAt 5 20
                        <|  case focused root path of
                        -- FIXME: unfolds all the structure from root for every prop
                        FocusedBy level -> "focused " ++ String.fromInt level
                        NotFocused -> ""
                    , positionAt 0 30
                        <| propertyDebug ( label, prop )
                    ]
            Fancy -> Svg.g [] []


viewPlate : Bounds -> Html msg
viewPlate plateBounds =
    positionAt_ (B.multiplyBy cellWidth <| plateBounds) <|
        case mode of
            Debug ->
                S.g [ SA.class "plate--debug" ]
                    [ rect_ "beige" <| B.multiplyBy cellWidth <| plateBounds
                    , boundsDebug <| B.multiplyBy cellWidth <| plateBounds
                    ]
            Fancy -> Svg.g [] []


view : Bounds -> Property msg -> Layout -> Html Msg
view bounds root layout =
    let
        keyDownHandler_ =
            H.on "keydown"
                <| Json.map KeyDown H.keyCode
        ( plates, cells ) =
            BinPack.unfold
                (\( cell, cellBounds ) ( prevPlates, prevCells ) ->
                    case cell of
                        One path ->
                            ( prevPlates
                            , case root |> Property.find1 path of
                                Just prop ->
                                    viewProperty root path cellBounds prop
                                        :: prevCells
                                Nothing ->
                                    prevCells
                            )
                        Plate plateLayout ->
                            ( viewPlate cellBounds :: prevPlates
                            , BinPack.unfold
                                (\ ( path, propBounds ) pPrevCells ->
                                    case root |> Property.find1 path of
                                        Just prop ->
                                            let
                                                shiftedBounds = B.shift cellBounds propBounds
                                            in
                                            viewProperty root path shiftedBounds prop
                                                :: pPrevCells
                                        Nothing ->
                                            pPrevCells
                                )
                                prevCells
                                plateLayout
                            )
                )
                ( [], [] )
                layout
    in
        div [ HA.id rootId
            , HA.class "gui gui--debug noselect"
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
                    [ Svg.g [] plates
                    , Svg.g [] cells
                    ]
                ]

            ]

