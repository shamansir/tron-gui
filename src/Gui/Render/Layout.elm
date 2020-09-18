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
import Gui.Render.Style as Style exposing (..)


type alias GridView = Html Msg


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


mode : Mode
mode = Fancy


viewProperty : Style.Mode -> Tone -> Path -> Bounds -> Focused -> ( Label, Property msg ) -> Svg Msg
viewProperty style tone path propBounds focus ( label, prop ) = -- FIXME: get rid of passing root
    let
        pixelBounds =
            B.multiplyBy cellWidth <| propBounds
    in
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g
                    [ SA.class "cell--debug"
                    , H.onClick <| Click path
                    ]
                    <| List.map (Svg.map <| always NoOp)
                    <| [ rect_ "white" pixelBounds
                    , boundsDebug propBounds
                    , textAt 5 20
                        <| case focus of
                            FocusedBy level -> "focused " ++ String.fromInt level
                            NotFocused -> ""
                    , positionAt 0 30
                        <| propertyDebug ( label, prop )
                    ]
            Fancy ->
                Property.view style tone path pixelBounds focus ( label, prop )


viewPlate : Style.Mode -> Bounds -> Svg msg
viewPlate style plateBounds =
    let
        pixelBounds =
            B.multiplyBy cellWidth <| plateBounds
    in
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ SA.class "plate--debug" ]
                    [ rect_ "beige" pixelBounds
                    , boundsDebug pixelBounds
                    ]
            Fancy ->
                Svg.rect
                    [ SA.fill <| background style
                    , SA.x <| String.fromFloat (gap / 2)
                    , SA.y <| String.fromFloat (gap / 2)
                    , SA.rx <| String.fromFloat borderRadius
                    , SA.ry <| String.fromFloat borderRadius
                    , SA.width <| String.fromFloat (pixelBounds.width - gap) ++ "px"
                    , SA.height <| String.fromFloat (pixelBounds.height - gap) ++ "px"
                    ]
                    []


view : Style.Mode -> Bounds -> Property msg -> Layout -> Html Msg
view styleMode bounds root layout =
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
                            , case root |> Property.find1 path of
                                Just prop ->
                                    viewProperty
                                        styleMode
                                        (toneOf path)
                                        path
                                        cellBounds
                                        (focused root path)
                                        prop
                                        :: prevCells
                                Nothing ->
                                    prevCells
                            )

                        Plate originPath plateLayout ->
                            ( viewPlate styleMode cellBounds :: prevPlates
                            , BinPack.unfold
                                (\ ( path, propBounds ) pPrevCells ->
                                    case root |> Property.find1 path of
                                        Just prop ->
                                            let
                                                shiftedBounds = B.shift cellBounds propBounds
                                            in
                                                viewProperty
                                                    styleMode
                                                    (toneOf path)
                                                    path
                                                    shiftedBounds
                                                    (focused root path)
                                                    prop
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

