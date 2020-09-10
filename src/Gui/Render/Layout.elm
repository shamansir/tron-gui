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

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (find)
import Gui.Msg exposing (..)
import Gui.Layout exposing (..)
import Gui.Focus exposing (Focused(..), focused)
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


positionAt : Float -> Float -> Svg Msg -> Svg Msg
positionAt x y s =
    Svg.g
        [ SA.style <| "transform: translate("
            ++ String.fromFloat x ++ "px,"
            ++ String.fromFloat y ++ "px)"
        ]
        [ s ]


positionAt_ : { a | x : Float, y : Float } -> Svg Msg -> Svg Msg
positionAt_ pos =
    positionAt pos.x pos.y


textAt : Float -> Float -> String -> Svg Msg
textAt x y string =
    positionAt x y
        <| S.text_ []
        <| List.singleton
        <| S.text string


rect_ : String -> { a | width : Float, height : Float } -> Svg Msg
rect_ fill b =
    Svg.rect
        [ SA.fill fill
        -- , S.x <| String.fromFloat b.x ++ "px"
        -- , S.y <| String.fromFloat b.y ++ "px"
        , SA.width <| String.fromFloat b.width ++ "px"
        , SA.height <| String.fromFloat b.height ++ "px"
        , SA.stroke "black"
        , SA.strokeWidth "1"
        ]
        []


boundsDebug : Bounds -> Svg Msg
boundsDebug b =
    S.g []
        [ textAt 5 5 <| "(" ++ String.fromFloat b.x ++ "," ++ String.fromFloat b.y ++ ")"
        , textAt 5 20 <| String.fromFloat b.width ++ "x" ++ String.fromFloat b.height
        ]


propertyDebug : ( Label, Property msg ) -> Svg Msg
propertyDebug ( label, prop )  =
    case prop of
        Nil  ->
            S.g []
                [ textAt 5 5 <| label ++ " ghost" ]
        Number (Control { min, step, max } val _) ->
            S.g []
                [ textAt 5 5 <| label ++ " knob: "
                , textAt 5 20
                    <| String.fromFloat min ++ "/"
                    ++ String.fromFloat step ++ "/"
                    ++ String.fromFloat max
                    ++ " " ++ String.fromFloat val ]
        Coordinate (Control ( xConf, yConf ) ( valX, valY ) _) ->
            S.g []
                [ textAt 5 5 <| "xy: " ++ label
                , textAt 5 20
                    <| String.fromFloat xConf.min ++ "/"
                    ++ String.fromFloat xConf.step ++ "/"
                    ++ String.fromFloat xConf.max
                    ++ " " ++ String.fromFloat valX
                    ++ " " ++ String.fromFloat yConf.min ++ "/"
                    ++ String.fromFloat yConf.step ++ "/"
                    ++ String.fromFloat yConf.max
                    ++ " " ++ String.fromFloat valY ]
        Toggle (Control _ val _) ->
            S.g []
                [ textAt 5 5 <| label ++ " toggle: "
                , textAt 5 20
                    <| if val == TurnedOn then "on" else "off"
                ]
        Color (Control _ color _) ->
            S.g []
                [ textAt 5 5 <| label ++ " color: " ++ color
                ]
        Text (Control _ value _) ->
            S.g []
                [ textAt 5 5 <| label ++ " text: " ++ value
                ]
        Action _ ->
            S.g []
                [ textAt 5 5 <| label ++ " button" ]
        Group (Control _ ( state, maybeFocus ) _) ->
            S.g []
                [ textAt 5 5 <| label ++ " nested: "
                , textAt 5 20
                    <| if state == Expanded then "expanded" else "collapsed"
                , textAt 5 35
                    <| "focus: " ++
                    case maybeFocus of
                        Just (Focus focus) -> String.fromInt focus
                        _ -> "none"
                ]
        Choice (Control _ ( state, selected ) _ ) ->
            S.g []
                [ textAt 5 5 <| label ++ " choice: "
                , textAt 5 20
                    <| if state == Expanded then "expanded" else "collapsed"
                , textAt 5 35
                    <| " selected: " ++ String.fromInt selected
                ]


view : Property msg -> Layout -> Html Msg
view root layout =
    let
        keyDownHandler_ =
            H.on "keydown"
                <| Json.map KeyDown H.keyCode
        debugSideInPx = 90
        renderProp path bounds ( label, prop ) =
            positionAt_ (B.multiplyBy debugSideInPx <| bounds)
                <| S.g
                    [ SA.class "cell--debug"
                    , H.onClick <| Click path
                    ]
                    [ rect_ "white"
                        <| B.multiplyBy debugSideInPx
                        <| bounds
                    , boundsDebug bounds
                    , textAt 0 30
                        <|  case focused root path of
                        -- FIXME: unfolds all the structure from root for every prop
                        FocusedBy level -> "focused " ++ String.fromInt level
                        NotFocused -> ""
                    , positionAt 0 60
                        <| propertyDebug ( label, prop )
                    ]
        renderPlate bounds =
            positionAt_ (B.multiplyBy debugSideInPx <| bounds)
                <| S.g [ SA.class "plate--debug" ]
                    [ rect_ "beige" <| B.multiplyBy debugSideInPx <| bounds
                    , boundsDebug <| B.multiplyBy debugSideInPx <| bounds
                    ]
        ( plates, cells ) =
            BinPack.unfold
                (\( cell, bounds ) ( prevPlates, prevCells ) ->
                    case cell of
                        One path ->
                            ( prevPlates
                            , case root |> Property.find1 path of
                                Just prop ->
                                    renderProp path bounds prop
                                        :: prevCells
                                Nothing ->
                                    prevCells
                            )
                        Plate plateLayout ->
                            ( renderPlate bounds :: prevPlates
                            , BinPack.unfold
                                (\ ( path, pBounds ) pPrevCells ->
                                    case root |> Property.find1 path of
                                        Just prop ->
                                            renderProp path (B.shift bounds pBounds) prop
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
                [ SA.width "100px"
                , SA.height "100px"
                , SA.class "grid"
                ]
                [ Svg.g [] plates
                , Svg.g [] cells
                ]
            ]

