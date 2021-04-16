module Tron.Render.Layout exposing (..)


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

import Bounds exposing (..)
import Bounds as B exposing (..)

import Tron.Path as Path exposing (Path)
import Tron.Control exposing (..)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (find)
import Tron.Msg exposing (..)
import Tron.Layout exposing (..)
import Tron.Layout as Layout exposing (fold)
import Tron.Focus exposing (Focused(..))
import Tron.Focus as Focus exposing (toString)
import Tron.FocusLogic as Focus exposing (focused)
import Tron.Detach as Detach exposing (Ability(..))
import Tron.Pages as Pages
import Size exposing (Size(..))

import Tron.Render.Util exposing (..)
import Tron.Render.Util as Svg exposing (none)
import Tron.Render.Debug exposing (..)
import Tron.Render.Property as Property exposing (..)
import Tron.Render.Plate as Plate exposing (..)

import Tron.Style.Logic as Style exposing (..)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS exposing (..)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Theme as Theme exposing (toString)
import Tron.Style.Dock exposing (Dock)
import Tron.Style.Dock as Dock exposing (firstCellAt, toString)
import Tron.Style.Placement exposing (Placement(..))
import Tron.Style.Selected exposing (Selected(..))
import Tron.Style.Cell as Cell


type alias GridView = Html Msg_


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


mode : Mode
mode = Fancy


viewProperty
    :  Theme
    -> State
    -> Path
    -> BoundsF
    -> Maybe ( Label, Property msg )
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg_
viewProperty
    theme
    ( ( _, focus, selected ) as state )
    path
    pixelBounds
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
                    , case selected of
                        Usual -> Svg.none
                        Selected -> textAt 5 55 "selected"
                    , positionAt 0 30
                        <| propertyDebug ( label, prop )
                    ]
            Fancy ->
                Property.view
                    theme
                    state
                    path
                    pixelBounds
                    maybeSelectedInside
                    cellShape
                    ( label, prop )


viewPlateBack : Theme -> BoundsF -> Svg Msg_
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
     : Detach.Ability
    -> Theme
    -> Path
    -> BoundsF
    -> ( Label, Property msg )
    -> Svg Msg_
viewPlateControls detach theme path pixelBounds ( label, source )  =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Plate.controls detach theme path pixelBounds ( label, source )

viewPagingControls
     : Theme
    -> Path
    -> BoundsF
    -> ( Pages.PageNum, Pages.Count )
    -> Svg Msg_
viewPagingControls path theme pixelBounds paging  =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Plate.paging path theme pixelBounds paging


collectPlatesAndCells -- FIXME: a complicated function, split into many
    :  Dock
    -> ( Path, Property msg )
    -> Layout
    ->
        ( List
            { path : Path
            , label : String
            , bounds : BoundsF
            , source : Property msg
            , pages : Pages.Count
            }
        , List
            { path : Path
            , label : String
            , bounds : BoundsF
            , parent : Maybe (Property msg)
            , source : Property msg
            , index : Maybe Int
            }
        )
collectPlatesAndCells dock ( rootPath, root ) ( size, bp ) =
    Layout.fold
        (\cell ( prevPlates, prevCells ) ->
            case cell of

                One ( cellBounds, cellPath ) ->
                    ( prevPlates
                    , case root |> Property.find1 (Path.sub rootPath cellPath) of
                        Just ( label, source ) ->
                            { path = cellPath
                            , label = label
                            , parent = Nothing
                            , bounds =
                                cellBounds
                                    --|> Dock.adaptBounds dock size
                                    |> B.multiplyByF Cell.width
                            , source = source
                            , index = Nothing
                            } :: prevCells
                        Nothing -> prevCells
                    )

                Many ( plateBounds, originPath ) innerPages ->

                    case root |> Property.find1 (Path.sub rootPath originPath) of
                        Just ( label, source ) ->
                            (
                                { path = originPath
                                , label = label
                                , bounds =
                                    plateBounds
                                        --|> Dock.adaptBounds dock size
                                        |> B.multiplyByF Cell.width
                                , source = source
                                , pages = Pages.count innerPages
                                } :: prevPlates
                            ,
                                (innerPages
                                    |> Pages.getCurrent
                                    |> Maybe.withDefault []
                                    |> List.map
                                        (\( cellBounds, cellPath ) ->
                                            case root
                                                |> Property.find1 (Path.sub rootPath cellPath) of
                                                Just ( cellLabel, cellSource ) ->
                                                    { path = cellPath
                                                    , label = cellLabel
                                                    , parent = Just source
                                                    , bounds =
                                                        cellBounds
                                                            --|> Dock.adaptBounds dock size
                                                            -- |> B.shift
                                                            --     (plateBounds
                                                            --         |> Dock.adaptBounds dock size
                                                            --     )
                                                            |> B.multiplyByF Cell.width
                                                    , source = cellSource
                                                    , index = Path.last cellPath
                                                    } |> Just
                                                Nothing -> Nothing
                                        )
                                    |> List.filterMap identity
                                ) ++ prevCells
                            )
                        Nothing -> ( prevPlates, prevCells )


        )
        ( [], [] )
        ( size, bp )


view
    :  Theme
    -> Dock
    -> BoundsF
    -> Detach.State
    -> Detach.GetAbility
    -> Property msg
    -> Layout
    -> Html Msg_
view theme dock bounds detach getDetachAbility root layout =
    let

        keyDownHandler_ =
            HE.on "keydown"
                <| Json.map KeyDown HE.keyCode

        rootPath =
            detach
                |> Detach.stateToMaybe
                |> Maybe.withDefault Path.start

        ( plates, cells ) =
            collectPlatesAndCells dock ( rootPath, root ) layout

        platesBacksRendered =
            plates
                |> List.map (.bounds >> viewPlateBack theme )

        rootCellsCount =
            cells
                |> List.map .path
                |> List.filter (\cellPath -> (Path.sub rootPath cellPath |> Path.howDeep) == 1)
                |> List.length

        cellsRendered =
            cells |> List.map
                (\cell ->

                    viewProperty
                        theme
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
                        (getSelected cell.source)
                        ( cell.parent
                            |> Maybe.andThen Property.getCellShape
                            |> Maybe.withDefault CS.default
                        )
                        ( cell.label, cell.source )

                )

        platesControlsRendered =
            plates
                |> List.map
                    (\plate ->
                        if plate.source
                            |> Property.getCellShape
                            |> Maybe.withDefault CS.default
                            |> CS.isSquare then
                            viewPlateControls
                                (getDetachAbility plate.path)
                                theme
                                plate.path
                                plate.bounds
                                ( plate.label, plate.source )
                        else Svg.none
                    )

        platesPagingRendered =
            plates |> List.map
                (\plate ->
                    case plate.pages of
                        1 -> Svg.none
                        n ->
                            viewPagingControls
                                theme
                                plate.path
                                plate.bounds
                                ( plate.source
                                    |> Property.getPageNum
                                    |> Maybe.withDefault 1
                                , n
                                )
                )

        detachButtonPos =
            case
                Dock.firstCellAt
                    dock
                    (layout |> Layout.size |> Size.toInt)
                    rootCellsCount
                    of
                ( x, y ) ->
                    ( Basics.toFloat x * Cell.width + Cell.gap
                    , Basics.toFloat y * Cell.height + Cell.gap
                    )

        makeClass =
            "gui noselect "
                ++
                    (case mode of
                        Debug -> "gui--debug "
                        _ -> ""
                    )
                ++ " gui--" ++ Theme.toString theme
                ++ " gui--" ++ Dock.toString dock

    in
        div

            [ HA.id rootId
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
                    , Svg.g [ SA.class "grid__plate-paging" ] platesPagingRendered
                    ,
                        case getDetachAbility rootPath of
                            CanBeDetached localUrl ->

                                Svg.g
                                    [ SA.class "grid__detach"
                                    ]
                                    [ Plate.detachButton
                                        theme
                                        rootPath
                                        localUrl
                                        detachButtonPos
                                    ]

                            CannotBeDetached ->
                                Svg.none
                    ]
                ]

            ]

