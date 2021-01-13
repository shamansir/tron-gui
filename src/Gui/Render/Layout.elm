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

import BinPack exposing (Bounds)
import Bounds exposing (..)
import Bounds as B exposing (..)

import Gui.Path as Path exposing (Path)
import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (find)
import Gui.Msg exposing (..)
import Gui.Layout exposing (..)
import Gui.Layout as Layout exposing (unfold)
import Gui.Focus exposing (Focused(..))
import Gui.Focus as Focus exposing (toString)
import Gui.FocusLogic as Focus exposing (focused)
import Gui.Detach exposing (ClientId, Detach)
import Gui.Detach as Detach exposing (isAttached)

import Gui.Render.Util exposing (..)
import Gui.Render.Util as Svg exposing (none)
import Gui.Render.Debug exposing (..)
import Gui.Render.Property as Property exposing (..)
import Gui.Render.Plate as Plate exposing (..)

import Gui.Style.Logic as Style exposing (..)
import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CS exposing (..)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Theme as Theme exposing (toString)
import Gui.Style.Dock exposing (Dock)
import Gui.Style.Dock as Dock exposing (firstCellAt, toString)
import Gui.Style.Placement exposing (Placement(..))
import Gui.Style.Selected exposing (Selected(..))
import Gui.Style.Tone as Tone exposing (none)
import Gui.Style.Cell as Cell


type alias GridView = Html Msg


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


mode : Mode
mode = Fancy


viewProperty
    :  Style
    -> State
    -> Path
    -> Bounds
    -> Maybe Bounds
    -> Maybe ( Label, Property msg )
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg
viewProperty
    style
    ( ( _, focus, _ ) as state )
    path
    pixelBounds
    parentPixelBounds
    maybeSelectedInside
    cellShape
    ( label, prop ) =
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
                    , textAt 5 20 <| Focus.toString focus
                    , positionAt 0 30
                        <| propertyDebug ( label, prop )
                    ]
            Fancy ->
                Property.view
                    style
                    state
                    path
                    pixelBounds
                    maybeSelectedInside
                    cellShape
                    ( label, prop )


viewPlateBack : Style -> Bounds -> Svg Msg
viewPlateBack style pixelBounds =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ SA.class "plate--debug" ]
                    [ rect_ "beige" pixelBounds
                    , boundsDebug pixelBounds
                    ]
            Fancy ->
                Plate.back style pixelBounds


viewPlateControls
     : Detach msg
    -> Style
    -> Path
    -> Bounds
    -> ( Label, Property msg )
    -> Svg Msg
viewPlateControls detach style path pixelBounds  ( label, source )  =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Plate.controls detach style path pixelBounds ( label, source )


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
            , parentBounds: Maybe Bounds
            , parent : Maybe (Property msg)
            , source : Property msg
            , index : Maybe Int
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
                            , bounds = B.multiplyBy Cell.width cellBounds
                            , parentBounds = Nothing
                            , source = source
                            , index = Nothing
                            } :: prevCells
                        Nothing -> prevCells
                    )

                Many ( originPath, plateBounds ) innerCells ->

                    case root |> Property.find1 (Path.sub rootPath originPath) of
                        Just ( label, source ) ->
                            (
                                { path = originPath
                                , label = label
                                , bounds = B.multiplyBy Cell.width plateBounds
                                , source = source
                                } :: prevPlates
                            ,
                                (innerCells
                                    |> List.indexedMap
                                        (\index ( cellPath, cellBounds ) ->
                                            case root
                                                |> Property.find1 (Path.sub rootPath cellPath) of
                                                Just ( cellLabel, cellSource ) ->
                                                    { path = cellPath
                                                    , label = cellLabel
                                                    , parent = Just source
                                                    , bounds = B.multiplyBy Cell.width cellBounds
                                                    , parentBounds = Just plateBounds
                                                    , source = cellSource
                                                    , index = Just index
                                                    } |> Just
                                                Nothing -> Nothing
                                        )
                                    |> List.filterMap identity
                                ) ++ prevCells
                            )
                        Nothing -> ( prevPlates, prevCells )


        )
        ( [], [] )


view : Theme -> Dock -> Bounds -> Detach msg -> Property msg -> Layout -> Html Msg
view theme dock bounds detach root layout =
    let

        keyDownHandler_ =
            HE.on "keydown"
                <| Json.map KeyDown HE.keyCode

        rootPath =
            Detach.isAttached detach
                |> Maybe.withDefault Path.start
        tones = Style.assignTones root

        toneOf path =
            tones |> Dict.get (Path.toString path) |> Maybe.withDefault Tone.none

        ( plates, cells ) =
            collectPlatesAndCells ( rootPath, root ) layout

        ( platesBacksRendered, cellsRendered, platesControlsRendered ) =

            ( plates |> List.map (.bounds >> viewPlateBack ( theme, Tone.none ) )

            , cells |> List.map
                (\cell ->
                    viewProperty
                        ( theme, toneOf cell.path )
                        ( if (Path.sub rootPath cell.path |> Path.howDeep) == 1
                            then AtRoot
                            else OnAPlate
                        , focused root cell.path
                        , if Maybe.map2 isSelected cell.parent cell.index
                                |> Maybe.withDefault False
                            then Selected
                            else Usual
                        )
                        cell.path
                        cell.bounds
                        cell.parentBounds
                        (getSelected cell.source)
                        ( cell.parent
                            |> Maybe.andThen Property.getCellShape
                            |> Maybe.withDefault CS.default
                        )
                        ( cell.label, cell.source )
                )

            , plates |> List.map
                (\plate ->
                    if plate.source
                        |> Property.getCellShape
                        |> Maybe.withDefault CS.default
                        |> CS.isSquare then
                        viewPlateControls
                            detach
                            ( theme, toneOf plate.path )
                            plate.path
                            plate.bounds
                            ( plate.label, plate.source )
                    else Svg.none
                )
            )

        detachButtonPos =
            case Dock.firstCellAt dock bounds of
                ( x, y ) -> ( x + Cell.gap, y + Cell.gap )

        makeClass =
            "gui noselect "
                ++ (case mode of
                    Debug -> "gui--debug "
                    _ -> "")
                ++ " gui--" ++ Theme.toString theme
                ++ " gui--" ++ Dock.toString dock

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
                    [ Svg.g [ SA.class "grid__backs" ] platesBacksRendered
                    , Svg.g [ SA.class "grid__cells" ] cellsRendered
                    , Svg.g [ SA.class "grid__plate-controls" ] platesControlsRendered

                    , case detach |> Detach.getLocalUrl rootPath of
                        Just localUrl ->

                            Svg.g
                                [ SA.class "grid__detach"
                                ]
                                [ Plate.detachButton
                                    ( theme, Tone.none )
                                    rootPath
                                    localUrl
                                    detachButtonPos
                                ]

                        Nothing -> Svg.none
                    ]
                ]

            ]

