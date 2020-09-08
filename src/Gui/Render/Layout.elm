module Gui.Render.Layout exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Json.Decode as Json

import BinPack exposing (Bounds)

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (find)
import Gui.Msg exposing (..)
import Gui.Layout exposing (..)
import Gui.Render.Control as Control exposing (..)


type alias GridView = Html Msg


rootId : String
rootId = "grid-gui"


type Mode
    = DebugInfo
    | Fancy


mode : Mode
mode = Fancy

{- }
viewCellContentDebug : GridPos -> GridCell umsg -> GridView umsg
viewCellContentDebug ((GridPos row col) as gridPos) { cell, nestPos, isSelected } =
    let
        posStr = showGridPos gridPos ++ " " ++ showNestPos nestPos
    in case cell of
        Ghost label  ->
            span []
                [ text <| posStr ++ " ghost: " ++ label ]
        Knob label { min, step, max } val _ ->
            span []
                [ text <| posStr ++ " knob: " ++ label
                    ++ " " ++ String.fromFloat min ++ "/"
                    ++ String.fromFloat step ++ "/"
                    ++ String.fromFloat max
                    ++ " " ++ String.fromFloat val ]
        XY label ( xConf, yConf ) ( valX, valY ) _ ->
            span []
                [ text <| posStr ++ " xy: " ++ label
                    ++ " " ++ String.fromFloat xConf.min ++ "/"
                    ++ String.fromFloat xConf.step ++ "/"
                    ++ String.fromFloat xConf.max
                    ++ " " ++ String.fromFloat valX
                    ++ " " ++ String.fromFloat yConf.min ++ "/"
                    ++ String.fromFloat yConf.step ++ "/"
                    ++ String.fromFloat yConf.max
                    ++ " " ++ String.fromFloat valY ]
        Toggle label val _ ->
            span []
                [ text <| posStr ++ " toggle: " ++ label ++ " "
                    ++ (if val == TurnedOn then "on" else "off")
                ]
        Button label _ ->
            span []
                [ text <| posStr ++ " button: " ++ label ]
        Nested label state _ ->
            span []
                [ text <| posStr ++ " nested: " ++ label ++ " "
                    ++ (if state == Expanded then "expanded" else "collapsed")
                ]
        -- NestedItem level cell ->
        --     span [ ]
        --         [ text <| showPos pos ++ " nested item: " ++ toString level ++ " "
        --         , viewCell_ pos cell
        --         ]
        Choice label selected id _ _ ->
            span []
                [ text <| posStr ++ " choice: " ++ label ++ " "
                    ++ String.fromInt id
                ]
        ChoiceItem label ->
            span []
                [ text <| posStr ++ " choiceitem: " ++ label ++ " "
                    ++ (if isSelected == Just Selected then "selected" else "not-selected")
                ]


viewCellContent : Focus -> GridPos -> GridCell umsg -> GridView umsg
viewCellContent focus gridPos gridCell =
    case mode of
        DebugInfo -> viewCellContentDebug gridPos gridCell
        Fancy ->
            case gridCell of
                { cell, nestPos, isSelected }
                    -> renderCell nestPos focus isSelected cell


viewCell : Focus -> GridPos -> Maybe (GridCell umsg) -> GridView umsg
viewCell focus gridPos maybeGridCell =
    let
        findFocusIntensity cellNestLevel focusNestLevel =
            focusNestLevel - cellNestLevel
        getFocusIntensityClass cellNestLevel (Focus innerFocus) =
            "focused--" ++ String.fromInt
                (findFocusIntensity cellNestLevel <| getNestLevel innerFocus)
        getLevelIntensityClass cellNestLevel (Focus innerFocus) =
            "level--" ++ String.fromInt
                (findFocusIntensity cellNestLevel <| getNestLevel innerFocus)
        className =
            case maybeGridCell of
                Just { isSelected, isFocused, nestPos } ->
                    (case ( isSelected, isFocused ) of
                        ( Just Selected, Focused nestLevel ) ->
                            "cell selected focused " ++
                                getFocusIntensityClass nestLevel focus
                        ( Just Selected, NotFocused ) -> "cell selected"
                        ( Just NotSelected, Focused nestLevel ) ->
                            "cell focused " ++
                                getFocusIntensityClass nestLevel focus
                        ( Nothing, Focused nestLevel ) ->
                            "cell focused " ++
                                getFocusIntensityClass nestLevel focus
                        _ -> "cell")
                            ++ " " ++ getLevelIntensityClass
                                        (getNestLevel nestPos) focus
                _ -> "cell hole"
        handlers =
            maybeGridCell
                |> Maybe.map
                    (\gridCell ->
                        [ H.onClick <| Click gridCell
                        , H.onMouseDown <| MouseDown gridCell
                        ]
                    )
                |> Maybe.withDefault []
        attributes = [ H.class className ] ++ handlers
        children = maybeGridCell
            |> Maybe.map (\cell -> [ viewCellContent focus gridPos cell ])
            |> Maybe.withDefault []
    in
        div attributes children


viewRow : Focus -> GridPos -> Row umsg -> GridView umsg
viewRow focus (GridPos row col) cols =
    Array.indexedMap
        (\subCol -> viewCell focus (GridPos row (col + subCol)))
        cols
        |> Array.toList
        |> div [ H.class "row" ]


viewRows : Focus -> Rows umsg -> GridView umsg
viewRows focus rows =
    let
        origin  = bottomLeft
        (GridPos row col) = origin
        topRows =
            rows
                |> Array.indexedMap
                    (\subRow -> viewRow focus (GridPos (row + subRow) col))
                |> Array.toList
    in
        topRows |> div [ H.class "cells" ]


viewGrid : Focus -> Grid umsg -> GridView umsg
viewGrid focus ((Grid _ rows) as grid) =
    let
        ( width, _ ) = getSizeInPixels grid
    in
        div [ H.class "grid"
            , H.style "width" (String.fromInt width ++ "px")
            ]
            [ rows |> viewRows focus ]



showGridPos : GridPos -> String
showGridPos (GridPos row col) =
    "(" ++ String.fromInt row ++ "," ++ String.fromInt col ++ ")"


showNestPos : NestPos -> String
showNestPos (NestPos path) =
    "<" ++ (path |> List.reverse |> List.map String.fromInt |> String.join ",") ++ ">"


view : Nest umsg -> GridView umsg
view nest =
    let
        (Focus focus) = Focus.get nest
        -- _ = Debug.log "nest" nest
        -- _ = Debug.log "focus" focus
        grid = layout nest
        -- cellCount = sizeOf nest
        --keyDownHandlers = Json.map (\_ -> [ NoOp ]) H.keyCode
        maybeCurrentCell =
            findGridCell focus grid
        parentFocus = focus |> shallower
        maybeParentCell = findGridCell parentFocus grid

        keyDownHandler_ = H.on "keydown"
            <| Json.map
                (\key ->
                    KeyDown key (Focus focus)
                        <| Maybe.map2
                            (\cur par -> { current = cur, parent = par })
                            maybeCurrentCell
                            maybeParentCell )
                H.keyCode
    in
        div [ H.id rootId
            , H.class "gui noselect"
            , H.tabindex 0
            , keyDownHandler_
            ]
            [ grid |> viewGrid (Focus focus) ]
-}

boundsDebug : Bounds -> Html Msg
boundsDebug b =
    span []
        [ text "x: ", text <| String.fromFloat b.x
        , text "y: ", text <| String.fromFloat b.y
        , text "w: ", text <| String.fromFloat b.width
        , text "h: ", text <| String.fromFloat b.height
        ]


propertyDebug : ( Label, Property msg ) -> Html Msg
propertyDebug ( label, prop )  =
    case prop of
        Nil  ->
            span []
                [ text <| label ++ " ghost" ]
        Number (Control { min, step, max } val _) ->
            span []
                [ text <| label ++ " knob: "
                    ++ " " ++ String.fromFloat min ++ "/"
                    ++ String.fromFloat step ++ "/"
                    ++ String.fromFloat max
                    ++ " " ++ String.fromFloat val ]
        Coordinate (Control ( xConf, yConf ) ( valX, valY ) _) ->
            span []
                [ text <| "xy: " ++ label
                    ++ " " ++ String.fromFloat xConf.min ++ "/"
                    ++ String.fromFloat xConf.step ++ "/"
                    ++ String.fromFloat xConf.max
                    ++ " " ++ String.fromFloat valX
                    ++ " " ++ String.fromFloat yConf.min ++ "/"
                    ++ String.fromFloat yConf.step ++ "/"
                    ++ String.fromFloat yConf.max
                    ++ " " ++ String.fromFloat valY ]
        Toggle (Control _ val _) ->
            span []
                [ text <| label ++ " toggle: "
                    ++ (if val == TurnedOn then "on" else "off")
                ]
        Color (Control _ color _) ->
            span []
                [ text <| label ++ " color: " ++ color
                ]
        Text (Control _ value _) ->
            span []
                [ text <| label ++ " text: " ++ value
                ]
        Action _ ->
            span []
                [ text <| label ++ " button" ]
        Group (Control _ ( state, maybeFocus ) _) ->
            span []
                [ text <| label ++ " nested: "
                    ++ (if state == Expanded then "expanded" else "collapsed")
                    ++ " focus: " ++ (case maybeFocus of
                        Just (Focus focus) -> String.fromInt focus
                        _ -> "none")
                ]
        Choice (Control _ ( state, selected ) _ ) ->
            span []
                [ text <| label ++ " choice: "
                    ++ (if state == Expanded then "expanded" else "collapsed")
                    ++ " selected: " ++ String.fromInt selected
                ]


view : Property msg -> Layout -> Html Msg
view root layout =
    let
        keyDownHandler_ =
            H.on "keydown"
                <| Json.map KeyDown H.keyCode
        renderProp path bounds ( label, prop ) =
            div
                [ H.style "pointer-events" "all"
                , H.onClick <| Click path
                ]
                [ boundsDebug bounds
                , propertyDebug ( label, prop )
                ]
        renderPlate bounds plateCells =
            div
                []
                <| boundsDebug bounds :: plateCells
        cells =
            BinPack.unfold
                (\ ( c, bounds ) prevCells ->
                    case c of
                        One path ->
                            case root |> Property.find1 path of
                                Just prop ->
                                    renderProp path bounds prop
                                        :: prevCells
                                Nothing ->
                                    prevCells
                        Plate plateLayout ->
                            [
                                renderPlate bounds
                                    <| BinPack.unfold
                                        (\ ( path, pBounds ) pPrevCells ->
                                            case root |> Property.find1 path of
                                                Just prop ->
                                                    renderProp path bounds prop
                                                        :: pPrevCells
                                                Nothing ->
                                                    pPrevCells
                                        )
                                        prevCells
                                        plateLayout
                            ]
                )
                []
                layout
    in
        div [ H.id rootId
            , H.class "gui noselect"
            , H.tabindex 0
            , keyDownHandler_
            ]
            cells
