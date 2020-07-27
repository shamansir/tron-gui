module Gui.Render.Grid exposing (..)


import Array exposing (..)
import Html exposing (Html, text, div, span, input)
import Html.Attributes as H
import Html.Events as H
import Json.Decode as Json


import Gui.Def exposing (..)
import Gui.Msg exposing (..)
import Gui.Nest exposing (..)
import Gui.Grid exposing (..)
import Gui.Render.Cell exposing (..)


type alias GridView umsg = Html ( Msg, Maybe umsg )


type Mode
    = DebugInfo
    | Fancy


mode : Mode
mode = Fancy


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
                        |> Html.map (\msg -> ( msg, Nothing ))


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
                        [ H.onClick <| doCellPurpose gridCell
                        , H.onMouseDown <| ( maybeFocus gridCell, Nothing )
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


viewGrid : Int -> Focus -> Grid umsg -> GridView umsg
viewGrid cellCount focus (Grid _ grid) =
    let
        width = (cellCount * (cellWidth + 2)) + (cellCount * cellMargin * 2)
    in
        div [ H.class "grid"
            , H.style "width" (String.fromInt width ++ "px")
            ]
            [ grid |> viewRows focus ]



showGridPos : GridPos -> String
showGridPos (GridPos row col) =
    "(" ++ String.fromInt row ++ "," ++ String.fromInt col ++ ")"


showNestPos : NestPos -> String
showNestPos (NestPos path) =
    "<" ++ (path |> List.reverse |> List.map String.fromInt |> String.join ",") ++ ">"


keyDownHandler : Nest umsg -> Grid umsg -> Int -> ( Msg, Maybe umsg )
keyDownHandler nest grid keyCode =
    let
        (Focus currentFocus) = findFocus nest
        maybeCurrentCell = findGridCell currentFocus grid
        executeCell = maybeCurrentCell
            |> Maybe.map doCellPurpose
            |> Maybe.withDefault ( NoOp, Nothing )
    -- Find top focus, with it either doCellPurpose or ShiftFocusRight/ShiftFocusLeft
    in
        case keyCode of
            -- left arrow
            37 -> ( ShiftFocusLeftAt currentFocus, Nothing )
            -- right arrow
            39 -> ( ShiftFocusRightAt currentFocus, Nothing )
            -- up arrow
            -- 38 -> ShiftFocusUpAt currentFocus
            -- down arrow
            -- 40 -> ShiftFocusDownAt currentFocus
            -- up arrow
            38 -> maybeCurrentCell
                |> Maybe.map (\{ cell } ->
                        ( case cell of
                            Nested _ Collapsed _ -> ExpandNested currentFocus
                            Choice _ Collapsed _ _ _ -> ExpandChoice currentFocus
                            _ -> NoOp
                        , Nothing
                        )
                    )
                |> Maybe.withDefault ( NoOp, Nothing ) -- execute as well?
            -- down arrow
            40 -> let parentFocus = currentFocus |> shallower in
                ( if (isSamePos parentFocus nowhere)
                    then NoOp
                    else
                        findGridCell parentFocus grid
                            |> Maybe.map (\{ cell } ->
                                    case cell of
                                        Nested _ Expanded _ -> CollapseNested parentFocus
                                        Choice _ Expanded _ _ _ -> CollapseChoice parentFocus
                                        _ -> NoOp
                                )
                            |> Maybe.withDefault NoOp
                , Nothing
                )
            -- space
            33 -> executeCell
            -- enter
            13 -> executeCell
            -- else
            _ -> ( NoOp, Nothing )


view : Nest umsg -> GridView umsg
view nest =
    let
        grid = layout nest
        focus = findFocus nest
        cellCount = sizeOf nest
        --keyDownHandlers = Json.map (\_ -> [ NoOp ]) H.keyCode
        keyDownHandler_ = H.on "keydown" <| Json.map (keyDownHandler nest grid) H.keyCode
    in
        div [ H.id "grid-gui"
            , H.class "gui noselect"
            , H.tabindex -1
            , keyDownHandler_
            ]
            [ grid |> viewGrid cellCount focus ]
