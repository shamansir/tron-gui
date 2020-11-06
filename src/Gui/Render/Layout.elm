module Gui.Render.Layout exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input, br)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Html.Events as HE
import Json.Decode as Json
import Dict
import Url exposing (Url)

import BinPack
import Bounds exposing (..)
import Bounds as B exposing (..)

import Gui.Path as Path exposing (Path)
import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (find)
import Gui.Msg exposing (..)
import Gui.Layout exposing (..)
import Gui.Layout as Layout exposing (unfold)
import Gui.Focus exposing (Focused(..), focused)
import Gui.Detach exposing (ClientId, Detach)
import Gui.Detach as Detach exposing (isAttached)

import Gui.Render.Util exposing (..)
import Gui.Render.Debug exposing (..)
import Gui.Render.Property as Property exposing (..)
import Gui.Render.Plate as Plate exposing (..)
import Gui.Render.Style as Style exposing (..)
import Gui.Render.StyleLogic as Style exposing (..)


type alias GridView = Html Msg


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


mode : Mode
mode = Fancy


viewProperty
    :  Property.Placement
    -> Style.Theme
    -> Tone
    -> Path
    -> Bounds
    -> Focused
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg
viewProperty placement theme tone path pixelBounds focus cellShape ( label, prop ) =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g
                    [ SA.class "cell--debug"
                    , HE.onClick <| Click path
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
                Property.view placement theme tone path pixelBounds focus cellShape ( label, prop )


viewPlateBack : Theme -> Bounds -> Svg Msg
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


viewPlateControls
     : Detach msg
    -> Theme
    -> Tone
    -> Path
    -> Bounds
    -> ( Label, Property msg )
    -> Svg Msg
viewPlateControls detach theme tone path pixelBounds  ( label, source )  =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Plate.controls detach theme tone path pixelBounds ( label, source )


collectPlatesAndCells -- FIXME: a complicated function, split into many
    :  ( Path, Property msg )
    -> Layout
    ->
        ( List
            { path : Path
            , label : String
            , bounds : Bounds
            , source : Property msg
            }
        , List
            { path : Path
            , label : String
            , bounds : Bounds
            , parent : Maybe (Property msg)
            , source : Property msg
            }
        )
collectPlatesAndCells ( rootPath, root ) =
    Layout.unfold
        (\cell ( prevPlates, prevCells ) ->
            case cell of

                One ( cellPath, cellBounds ) ->
                    ( prevPlates
                    , case root |> Property.find1 (Path.sub rootPath cellPath) of
                        Just ( label, source ) ->
                            { path = cellPath
                            , label = label
                            , parent = Nothing
                            , bounds = B.multiplyBy cellWidth cellBounds
                            , source = source
                            } :: prevCells
                        Nothing -> prevCells
                    )

                Many ( originPath, plateBounds ) innerCells ->

                    case root |> Property.find1 (Path.sub rootPath originPath) of
                        Just ( label, source ) ->
                            (
                                { path = originPath
                                , label = label
                                , bounds = B.multiplyBy cellWidth plateBounds
                                , source = source
                                } :: prevPlates
                            ,
                                (innerCells
                                    |> List.map
                                        (\( cellPath, cellBounds ) ->
                                            case root
                                                |> Property.find1 (Path.sub rootPath cellPath) of
                                                Just ( cellLabel, cellSource ) ->
                                                    { path = cellPath
                                                    , label = cellLabel
                                                    , parent = Just source
                                                    , bounds = B.multiplyBy cellWidth cellBounds
                                                    , source = cellSource
                                                    } |> Just
                                                Nothing -> Nothing
                                        )
                                    |> List.filterMap identity
                                ) ++ prevCells
                            )
                        Nothing -> ( prevPlates, prevCells )


        )
        ( [], [] )


view : Style.Theme -> Style.Flow -> Bounds -> Detach msg -> Property msg -> Layout -> Html Msg
view theme flow bounds detach root layout =
    let

        keyDownHandler_ =
            HE.on "keydown"
                <| Json.map KeyDown HE.keyCode

        rootPath =
            Detach.isAttached detach
                |> Maybe.withDefault Path.start
        tones = Style.assignTones root

        toneOf path =
            tones |> Dict.get (Path.toString path) |> Maybe.withDefault None

        ( plates, cells ) =
            collectPlatesAndCells ( rootPath, root ) layout

        ( platesBacksRendered, cellsRendered, platesControlsRendered ) =

            ( plates |> List.map (.bounds >> viewPlateBack theme)

            , cells |> List.map
                (\cell ->
                    viewProperty
                        (if (Path.sub rootPath cell.path |> Path.howDeep) == 1
                            then AtRoot
                            else OnAPlate
                        )
                        theme
                        (toneOf cell.path)
                        cell.path
                        cell.bounds
                        (focused root cell.path)
                        ( cell.parent
                            |> Maybe.andThen Property.getCellShape
                            |> Maybe.withDefault Full
                        )
                        ( cell.label, cell.source )
                )

            , plates |> List.map
                (\plate ->
                    case plate.source |> Property.getCellShape of
                        Just Full ->
                            viewPlateControls
                                detach
                                theme
                                (toneOf plate.path)
                                plate.path
                                plate.bounds
                                ( plate.label, plate.source )
                        _ ->
                            Svg.g [] []
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

                    , case detach |> Detach.getLocalUrl rootPath of
                        Just localUrl ->

                            Svg.g
                                [ SA.style <| " pointer-events: all; cursor: pointer; transform: translate(" ++
                                String.fromFloat gap ++ "px," ++ String.fromFloat gap ++ "px);"
                                , HE.onClick <| Detach rootPath
                                ]
                                [ Svg.a
                                    [ SA.xlinkHref <| Detach.localUrlToString localUrl
                                    , SA.target "_blank"
                                    ]
                                    [ Svg.rect
                                        [ SA.fill "transparent"
                                        , SA.x "0"
                                        , SA.y "2.5"
                                        , SA.width "10"
                                        , SA.height "10"
                                        ]
                                        []
                                    , Plate.detach theme Style.None
                                    ]
                                ]

                        Nothing -> Svg.g [] []
                    ]
                ]

            ]

