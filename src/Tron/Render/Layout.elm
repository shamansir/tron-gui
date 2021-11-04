module Tron.Render.Layout exposing (..)


import Array exposing (..)
import Html exposing (Html, div)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Html.Events as HE
import Json.Decode as Json

import Bounds exposing (..)
import Bounds as B exposing (..)

import Tron.Path as Path exposing (Path)
import Tron.Control exposing (..)
import Tron.Tree as Tree exposing (Tree)
import Tron.Tree.Paths as Tree
import Tron.Tree.Controls as Tree
import Tron.Msg exposing (..)
import Tron.Layout exposing (..)
import Tron.Layout as Layout
import Tron.Focus exposing (Focused(..))
import Tron.Focus as Focus
import Tron.FocusLogic as Focus exposing (focused)
import Tron.Detach as Detach exposing (Ability(..))
import Tron.Pages as Pages
import Size exposing (Size(..))

import Tron.Render.Context exposing (Context)
import Tron.Render.Util exposing (..)
import Tron.Render.Util as Svg
import Tron.Render.Debug exposing (..)
import Tron.Render.Tree as Tree exposing (..)
import Tron.Render.Plate as Plate exposing (..)

import Tron.Style.Logic exposing (..)
import Tron.Style.Logic as Dock
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS exposing (..)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Theme as Theme
import Tron.Style.Dock exposing (Dock)
import Tron.Style.Dock as Dock
import Tron.Style.Placement exposing (Placement(..))
import Tron.Style.Selected exposing (Selected(..))
import Tron.Style.Cell as Cell


type alias GridView = Html Msg_


rootId : String
rootId = "grid-gui"



type Mode
    = Debug
    | Fancy


viewTree
    :  Mode
    -> Theme
    -> Context
    -> Path
    -> Maybe ( Path.Label, Tree a )
    -> ( Path.Label, Tree a )
    -> Svg Msg_
viewTree
    mode
    theme
    ( { focused, selected } as ctx )
    path
    maybeSelectedInside
    ( label, prop ) =
    positionAt_ ctx.bounds <|
        case mode of
            Debug ->
                S.g
                    [ SA.class "cell--debug"
                    , HE.onClick <| Click path
                    ]
                    <| List.map (Svg.map <| always NoOp)
                    <| [ rect_ "white" ctx.bounds
                    , boundsDebug ctx.bounds -- FIXME: were in cells before, not in pixels
                    , textAt 5 20 <| Focus.toString focused
                    , case selected of
                        Usual -> Svg.none
                        Selected -> textAt 5 55 "selected"
                    , positionAt 0 30
                        <| propertyDebug ( label, prop )
                    ]
            Fancy ->
                Tree.view
                    theme
                    ctx
                    path
                    maybeSelectedInside
                    ( label, prop )


viewPlateBack : Mode -> Theme -> BoundsF -> Svg Msg_
viewPlateBack mode theme pixelBounds =
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
    -> Mode
    -> Theme
    -> Path
    -> BoundsF
    -> ( Path.Label, Tree a )
    -> Svg Msg_
viewPlateControls detach mode theme path pixelBounds ( label, source )  =
    positionAt_ pixelBounds <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Plate.controls detach theme path pixelBounds ( label, source )

viewPagingControls
     : Mode
    -> Theme
    -> Path
    -> BoundsF
    -> CellShape
    -> ( Pages.PageNum, Pages.Count )
    -> Svg Msg_
viewPagingControls mode theme path pixelBounds cellShape paging  =
    positionAt_ (pixelBounds |> Bounds.shiftF { x = 0, y = 5 }) <|
        case mode of
            Debug ->
                S.g [ ] [ ]
            Fancy ->
                Svg.svg
                    [ SA.width <| String.fromFloat pixelBounds.width
                    , SA.height <| String.fromFloat (pixelBounds.height + 5)
                    ]
                    [ paginationMaskDefs pixelBounds path
                    , S.g
                        [ SA.mask <| "url(#" ++ paginationMaskIdFor path ++ ")" ]
                        [ paging |> Plate.paging theme path pixelBounds cellShape
                        ]
                    ]


collectPlatesAndCells -- FIXME: a complicated function, split into many
    :  Dock
    -> ( Path, Tree a )
    -> Layout
    ->
        ( List
            { path : Path
            , bounds : BoundsF
            , source : Tree a
            , pages : Pages.Count
            }
        , List
            { path : Path
            , bounds : BoundsF
            , parent : Maybe (Tree a)
            , source : Tree a
            }
        )
collectPlatesAndCells dock ( rootPath, root ) ( size, bp ) =
    Layout.fold
        (\cell ( prevPlates, prevCells ) ->
            case cell of

                One ( cellBounds, cellPath ) ->
                    ( prevPlates
                    , case root |> Tree.find1 (Path.sub rootPath cellPath) of
                        Just ( label, source ) ->
                            { path = cellPath
                            , parent = Nothing
                            , bounds =
                                cellBounds
                                    --|> Dock.adaptBounds dock size
                                    |> B.multiplyByF Cell.width
                            , source = source
                            } :: prevCells
                        Nothing -> prevCells
                    )

                Many ( plateBounds, originPath ) innerPages ->

                    case root |> Tree.find1 (Path.sub rootPath originPath) of
                        Just ( label, source ) ->
                            (
                                { path = originPath
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
                                                |> Tree.find1 (Path.sub rootPath cellPath) of
                                                Just ( cellLabel, cellSource ) ->
                                                    { path = cellPath
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


maskDefs : Html msg
maskDefs =
    Svg.defs [ ]
        [ Svg.linearGradient
            [ SA.id "button-mask-gradient" ]
            [ Svg.stop [ SA.offset "0.0", SA.stopColor "white", SA.stopOpacity "0" ] []
            , Svg.stop [ SA.offset "0.2", SA.stopColor "white", SA.stopOpacity "1" ] []
            , Svg.stop [ SA.offset "0.8", SA.stopColor "white", SA.stopOpacity "1" ] []
            , Svg.stop [ SA.offset "1.0", SA.stopColor "white", SA.stopOpacity "0" ] []
            ]
        , Svg.mask
            [ SA.id "button-text-mask" ]
            [ Svg.rect
                [ SA.fill "url(#button-mask-gradient)"
                , SA.width <| String.fromFloat Cell.width
                , SA.height <| String.fromFloat Cell.height
                , SA.x "0", SA.y "0"
                ]
                []
            ]
        , Svg.mask
            [ SA.id "button-text-mask-wide" ]
            [ Svg.rect
                [ SA.fill "url(#button-mask-gradient)"
                , SA.width <| String.fromFloat (Cell.width * 2)
                , SA.height <| String.fromFloat Cell.height
                , SA.x "0", SA.y "0"
                ]
                []
            ]
        ]


paginationMaskDefs : BoundsF -> Path -> Html msg
paginationMaskDefs bounds path =
    Svg.defs [ ]
        [ Svg.mask
            [ SA.id <| paginationMaskIdFor path ]
            [ Svg.rect
                [ SA.fill "white"
                , SA.width <| String.fromFloat bounds.width -- "100%" --<| String.fromFloat (Cell.width * 3)
                , SA.height <| String.fromFloat bounds.height -- "100%" -- <| String.fromFloat (Cell.height * 3)
                , SA.x "0", SA.y "0", SA.rx "10", SA.ry "10"
                ]
                []
            , Svg.rect
                [ SA.fill "black"
                , SA.width <| String.fromFloat <| bounds.width - 10 -- "96%" --<| String.fromFloat (Cell.width * 3 - 12)
                , SA.height <| String.fromFloat <| bounds.height - 19 -- "93%" --<| String.fromFloat (Cell.height * 3 - 20)
                , SA.x "6", SA.y "6", SA.rx "8", SA.ry "8"
                ]
                []
            ]
        ]


view
    :  Mode
    -> Theme
    -> Dock
    -> BoundsF
    -> Detach.State
    -> Detach.GetAbility
    -> Tree a
    -> Layout
    -> Html Msg_
view mode theme dock bounds detach getDetachAbility root layout =
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
                |> List.map (.bounds >> viewPlateBack mode theme )

        rootCellsCount =
            cells
                |> List.map .path
                |> List.filter (\cellPath -> (Path.sub rootPath cellPath |> Path.howDeep) == 1)
                |> List.length

        cellsRendered =
            cells |> List.map
                (\cell ->

                    viewTree
                        mode
                        theme
                        { focused = focused root cell.path
                        , placement =
                            if (Path.sub rootPath cell.path |> Path.howDeep) == 1
                            then AtRoot
                            else OnAPlate
                        , selected =
                            if Maybe.map2 Tree.isSelected cell.parent (Path.lastIndex cell.path)
                            |> Maybe.withDefault False
                            then Selected
                            else Usual
                        , bounds = cell.bounds
                        , cellShape =
                            cell.parent
                            |> Maybe.andThen Tree.getCellShape
                            |> Maybe.withDefault CS.default
                        }
                        cell.path
                        (Tree.getSelected cell.source)
                        ( cell.path |> Path.lastLabel |> Maybe.withDefault "?"
                        , cell.source
                        )

                )

        platesControlsRendered =
            plates
                |> List.map
                    (\plate ->
                        if plate.source
                            |> Tree.getCellShape
                            |> Maybe.withDefault CS.default
                            |> CS.isSquare then
                            viewPlateControls
                                (getDetachAbility plate.path)
                                mode
                                theme
                                plate.path
                                plate.bounds
                                ( plate.path |> Path.lastLabel |> Maybe.withDefault "?"
                                , plate.source
                                )
                        else Svg.none
                    )

        platesPagingRendered =
            plates |> List.map
                (\plate ->
                    case plate.pages of
                        1 -> Svg.none
                        n ->
                            viewPagingControls
                                mode
                                theme
                                plate.path
                                plate.bounds
                                ( plate.source
                                    |> Tree.getCellShape
                                    |> Maybe.withDefault (Tree.defaultNestShape |> Tuple.second)
                                )
                                ( plate.source
                                    |> Tree.getPageNum
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
            -- , keyDownHandler_ -- moved to a top level
            ]

            [ Svg.svg

                [ SA.width <| String.fromFloat bounds.width ++ "px"
                , SA.height <| String.fromFloat bounds.height ++ "px"
                , SA.style <| "transform: translate("
                    ++ String.fromFloat bounds.x ++ "px,"
                    ++ String.fromFloat bounds.y ++ "px)"
                , SA.class "grid"
                ]

                [ maskDefs
                , Svg.g
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
