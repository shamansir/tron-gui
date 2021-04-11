module SmartPackDemo exposing (..)


import Browser
import Browser.Events
import Html exposing (Html, button, div, text, input, ul, li)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Task
import Json.Decode as D

import Array
import Size exposing (..)
import SmartPack exposing (SmartPack)
import SmartPack as SP
import Matrix exposing (..)


-- MAIN


main : Program () Model Msg
main =
    Browser.element
      { init = always init
      , update = update
      , view = view
      , subscriptions = subscriptions
      }


-- MODEL


type alias Color = String


type alias Pos = ( Int, Int )


type alias Rect =
    { width : Int
    , height : Int
    , color : Color
    }


defaultRect : Rect
defaultRect =
    { width = 4
    , height = 3
    , color = "aqua"
    }


nextColorFor color =
    case color of
        "aqua" -> "plum"
        "plum" -> "antiquewhite"
        "antiquewhite" -> "beige"
        "beige" -> "chocolate"
        "chocolate" -> "cornflowerblue"
        "cornflowerblue" -> "darkcyan"
        "darkcyan" -> "darkkhaki"
        "darkkhaki" -> "darkorange"
        "darkorange" -> "darkslategrey"
        "darkslategrey" -> "gold"
        "gold" -> "lightcoral"
        "lightcoral" -> "lightseagreen"
        "lightseagreen" -> "mediumturquoise"
        "mediumturquoise" -> "orangered"
        "orangered" -> "navajowhite"
        "navajowhite" -> "peru"
        "peru" -> "salmon"
        "salmon" -> "aqua"
        _ -> "aqua"


defaultSize : Size Cells
defaultSize = Size ( 20, 20 )


defaultDistribution : SP.Distribution
defaultDistribution = SP.Right


type RenderMode
    = Rects
    | Grid


type LeftMouseButton
    = LMBUp
    | LMBDown


type ShiftKey
    = ShiftKeyUp
    | ShiftKeyDown


type MouseAt
    = AtGrid ( Int, Int ) LeftMouseButton ShiftKey
    | AtRect ( Int, Int ) LeftMouseButton
    | Somewhere


type alias Model =
    { mode : RenderMode
    , smartPack : SmartPack Color
    , nextRect : Rect
    , nextPos : Maybe Pos
    , gridPreview : Maybe ( Pos, Rect )
    , rectPreview : Maybe Rect
    , distribution : SP.Distribution
    }


init : ( Model, Cmd Msg )
init =
    (
        { mode = Grid
        , smartPack = SP.container defaultSize
        --, mouse = Somewhere
        , gridPreview = Nothing
        , rectPreview = Nothing
        , nextRect = defaultRect
        , nextPos = Nothing
        , distribution = defaultDistribution
        }
    , Cmd.none
        {- Task.succeed ()
            |> Task.perform (always Randomize) -}
    )


-- UPDATE


type Msg
  = NoOp
  | ChangeMode RenderMode
  | Randomize
  | PackAll (List Rect)
  | PackOne Rect
  | PackOneAt ( Int, Int ) Rect
  | PackOneCloseTo ( Int, Int ) Rect
  | AddGridPreview ( Int, Int ) Rect
  | AddRectPreview Rect
  | SetNextRect Rect
  | SetNextRectWidth Int
  | SetNextRectHeight Int
  | SetNextRectX Int
  | SetNextRectY Int
  | SetGridWidth Int
  | SetGridHeight Int
  | ChangeDistribution SP.Distribution
  | Clear
  | Error Rect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
        ( model, Cmd.none )

    ChangeMode newMode ->
        (
            { model
            | mode = newMode
            }
        , Cmd.none
        )

    Randomize ->
       (
           { model
           | smartPack = SP.container defaultSize
           }
       , Random.generate
            PackAll
            random
       )

    PackAll rects ->
        (
            { model
            | smartPack =
                rects
                    |> List.foldl
                        (\{ width, height, color } ->

                            SP.carelessPack
                                model.distribution
                                (Size ( width, height))
                                color

                        )
                        model.smartPack
            }
        , Cmd.none
        )

    PackOne rect ->
        (
            { model
            | smartPack =

                model.smartPack
                    |> SP.carelessPack
                        model.distribution
                        (Size ( rect.width, rect.height ))
                        rect.color

            , gridPreview = Nothing
            , rectPreview = Nothing
            , nextRect =
                { rect
                | color = nextColorFor rect.color
                }
            }
        , Cmd.none
        )

    PackOneAt (x, y) rect ->
        (
            { model
            | smartPack =
                model.smartPack
                    |> SP.carelessPackAt (x, y) (Size ( rect.width, rect.height )) rect.color
            , gridPreview = Nothing
            , rectPreview = Nothing
            , nextRect =
                { rect
                | color = nextColorFor rect.color
                }
            }
        , Cmd.none
        )

    PackOneCloseTo (x, y) rect ->
        (
            { model
            | smartPack =
                model.smartPack
                    |> SP.carelessPackCloseTo
                        model.distribution
                        (x, y)
                        (Size ( rect.width, rect.height ))
                        rect.color
            , gridPreview = Nothing
            , rectPreview = Nothing
            , nextRect =
                { rect
                | color = nextColorFor rect.color
                }
            }
        , Cmd.none
        )

    SetNextRect rect ->
        (
            { model
            | nextRect = rect
            }
        , Cmd.none
        )

    SetNextRectWidth width ->
        (
            { model
            | nextRect =
                case model.nextRect of
                    r ->
                        { r
                        | width = width
                        }
            }
        , Cmd.none
        )

    SetNextRectHeight height ->
        (
            { model
            | nextRect =
                case model.nextRect of
                    r ->
                        { r
                        | height = height
                        }
            }
        , Cmd.none
        )


    SetNextRectX x ->
        (
            { model
            | nextPos =
                case model.nextPos of
                    Just (_, y) ->
                        Just (x, y)
                    Nothing ->
                        Just (x, 0)
            }
        , Cmd.none
        )

    SetNextRectY y ->
        (
            { model
            | nextPos =
                case model.nextPos of
                    Just (x, _) ->
                        Just (x, y)
                    Nothing ->
                        Just (0, y)
            }
        , Cmd.none
        )

    SetGridWidth width ->
        (
            { model
            | smartPack =
                case SP.dimensions model.smartPack of
                    (Size (_, height)) ->
                        model.smartPack
                            |> SP.resize (Size (width, height))
            }
        , Cmd.none
        )

    SetGridHeight height ->
        (
            { model
            | smartPack =
                case SP.dimensions model.smartPack of
                    (Size (width, _)) ->
                        model.smartPack
                            |> SP.resize (Size (width, height))
            }
        , Cmd.none
        )

    AddGridPreview pos rect ->
        (
            { model
            | gridPreview = Just ( pos, rect )
            , rectPreview = Nothing
            }
        , Cmd.none
        )

    AddRectPreview rect ->
        (
            { model
            | rectPreview = Just rect
            , gridPreview = Nothing
            }
        , Cmd.none
        )

    ChangeDistribution distribution ->
        (
            { model
            | distribution = distribution
            }
        , Cmd.none
        )

    Clear ->
        (
            { model
            | smartPack = SP.container defaultSize
            }
        , Cmd.none
        )

    Error rect ->
      ( model, Cmd.none )



-- VIEW

scale = 30


previewMarginX = 20
previewMarginY = 40


view : Model -> Html Msg
view model =
    let
        ( mainWidth, mainHeight ) =
            case SP.dimensions model.smartPack of
                Size ( w, h ) -> ( w, h )
        rect color bounds =
            Svg.rect
                [ S.x <| String.fromInt <| bounds.x * scale
                , S.y <| String.fromInt <| bounds.y * scale
                , S.width <| String.fromInt <| bounds.width * scale
                , S.height <| String.fromInt <| bounds.height * scale
                , S.fill color
                , S.strokeWidth "1"
                , S.stroke "black"
                ]
                []
        viewItem (bounds, color)
            = rect color bounds
        viewGridCell (x, y) maybeColor =
            Svg.g
                []
                [ Svg.rect
                    [ S.x <| String.fromInt <| x * scale
                    , S.y <| String.fromInt <| y * scale
                    , S.width <| String.fromFloat scale
                    , S.height <| String.fromFloat scale
                    , S.fill <| Maybe.withDefault "none" <| maybeColor
                    , S.strokeWidth "1"
                    , S.stroke "lightgray"
                    ]
                    []
                ]
        viewRectGrid (x, y) (width, height) background posToColor =
            Svg.g
                []
                <| rect
                    background
                    { x = x
                    , y = y
                    , width = width
                    , height = height
                    }
                :: (
                    Matrix.initialize (width, height) (always background)
                        |> Matrix.toIndexedList
                        |> List.map Tuple.first
                        |> List.map
                            (\(x_, y_)->
                                viewGridCell (x_, y_) <| posToColor (x_, y_)
                            )
                )
        rowsToCells rows =
            rows
                |> Array.indexedMap
                    (\y row ->
                        row
                            |> Array.indexedMap
                                (\x ( v, maybeColor ) ->
                                    { x = x
                                    , y = y
                                    , weight = v
                                    , value = maybeColor |> Maybe.withDefault "none"
                                    }
                                )
                    )
                |> Array.foldl Array.append Array.empty
    in
        div
            []
            [ case model.mode of
                Rects ->
                    svg [ S.width <| String.fromInt <| mainWidth * scale
                        , S.height <| String.fromInt <| mainHeight * scale
                        ]
                        <| List.map viewItem
                        <| SP.toList model.smartPack
                Grid ->
                    svg [ S.width <| String.fromInt <| mainWidth * scale
                        , S.height <| String.fromInt <| mainHeight * scale
                        ]
                        <| List.append

                            (List.concat
                                <| Matrix.toList
                                <| Matrix.indexedMap viewGridCell
                                <| SP.toMatrix model.smartPack
                            )

                            (case model.gridPreview of
                                Nothing -> []
                                Just ( pos, preview ) ->

                                    [ Svg.g
                                        [ S.style "cursor: pointer;" ]
                                        [ viewRectGrid
                                            pos
                                            (preview.width, preview.height)
                                            "darkgreen"
                                            (always Nothing)
                                        ]
                                    ]
                            )
            ,
                case model.nextRect of
                    { width, height, color } ->
                        case model.rectPreview of
                            Nothing ->
                                svg [ S.width <| String.fromInt <| width * scale
                                    , S.height <| String.fromInt <| height * scale
                                    , S.style "position: absolute; top: 40px; margin-left: 50px; cursor: pointer;"
                                    ]
                                    [ viewRectGrid
                                        (0, 0)
                                        (width, height)
                                        color
                                        (always Nothing)
                                    ]
                            Just preview ->
                                svg [ S.width <| String.fromInt
                                        <| (Basics.max preview.width width) * scale
                                    , S.height <| String.fromInt
                                        <| (Basics.max preview.height height) * scale
                                    , S.style "position: absolute; top: 40px; margin-left: 50px; cursor: pointer;"
                                    ]
                                    [ viewRectGrid
                                        (0, 0)
                                        (width, height)
                                        color
                                        (always Nothing)
                                    , viewRectGrid
                                        (0, 0)
                                        (preview.width, preview.height)
                                        "darkgreen"
                                        (always Nothing)
                                    ]
            , div
                [ ]
                [ input
                    [ H.type_ "button", onClick Randomize, H.value "Random" ]
                    [ Html.text "Random" ]
                , input
                    [ H.type_ "button", onClick Clear, H.value "Clear" ]
                    [ Html.text "Clear" ]
                ]
            , div
                []
                [ input
                    [ H.type_ "button", onClick <| ChangeMode Rects, H.value "Rectangles" ]
                    [ Html.text "Rectangles" ]
                , input
                    [ H.type_ "button", onClick <| ChangeMode Grid, H.value "Grid" ]
                    [ Html.text "Grid" ]
                ]
            , div
                []
                <| List.map
                    (\d ->
                        input
                            [ H.type_ "button"
                            , H.style "border" <|
                                if model.distribution == d then
                                    "1px solid black"
                                else "1px solid lightgray"
                            , onClick <| ChangeDistribution d
                            , H.value <| labelForDistribution d
                            ]
                            [ Html.text "Rectangles" ]
                    )
                <| [ SP.Up, SP.Down, SP.Right, SP.Left ]
            , div
                [ H.style "position" "fixed"
                , H.style "left" <| (String.fromInt <| mainWidth * scale + previewMarginX) ++ "px"
                , H.style "top" "5px"
                ]
                [ input
                    [ H.type_ "number"
                    , H.style "max-width" "25px"
                    , onInput
                        (String.toInt
                            >> Maybe.map SetNextRectWidth
                            >> Maybe.withDefault NoOp
                        )
                    , H.placeholder <| String.fromInt <| model.nextRect.width
                    , H.value <| String.fromInt <| model.nextRect.width
                    ]
                    [ ]
                , Html.text "x"
                , input
                    [ H.type_ "number"
                    , H.style "max-width" "25px"
                    , onInput
                        (String.toInt
                            >> Maybe.map SetNextRectHeight
                            >> Maybe.withDefault NoOp
                        )
                    , H.placeholder <| String.fromInt <| model.nextRect.height
                    , H.value <| String.fromInt <| model.nextRect.height
                    ]
                    [ ]
                , input
                    [ H.type_ "button"
                    , onClick (model.nextRect |> PackOne)
                    , H.value "Pack Rect"
                    ]
                    [ Html.text "Pack Rect" ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseMove
            (whereIsMouse model
                --|> D.map (Debug.log "mouseAt")
            )
        , Browser.Events.onMouseDown
            (whereIsMouse model
                --|> D.map (Debug.log "mouseDown")
            )
        ]
    |> Sub.map (mouseToMessage model)


-- RANDOM


randomColor : Random.Generator Color
randomColor =
    Random.map3
        (\r g b ->
            "rgb(" ++ String.fromFloat (r * 255) ++
            ","  ++ String.fromFloat (g * 255) ++
            ","  ++ String.fromFloat (b * 255) ++
            ")")
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


randomRect : Random.Generator Rect
randomRect =
    Random.map3
        Rect
        (Random.int 1 4)
        (Random.int 1 4)
        randomColor


random : Random.Generator (List Rect)
random =
    Random.int 2 15
      |> Random.andThen
          (\len -> Random.list len randomRect)


whereIsMouse : Model -> D.Decoder MouseAt
whereIsMouse model =
    D.map4
        (\pageX pageY buttons shiftKey ->
            ( ( pageX, pageY )
            , ( if buttons == 1 then LMBDown else LMBUp
              , if shiftKey then ShiftKeyDown else ShiftKeyUp
              )
            )
        )
        (D.field "pageX" D.float |> D.map floor)
        (D.field "pageY" D.float |> D.map floor)
        (D.field "buttons" D.int)
        (D.field "shiftKey" D.bool)
    |> D.map
        (\( (pageX, pageY), ( leftMouseButton, shiftKey ) ) ->
            case model.smartPack |> SP.dimensions of
                Size ( width, height ) ->
                    if pageX <= width * scale && pageY <= height * scale then
                        AtGrid
                            ( pageX // scale, pageY // scale )
                            leftMouseButton
                            shiftKey
                    else
                        if pageX > (width * scale + previewMarginX)
                        && pageY > previewMarginY
                        && pageY <= previewMarginY + height * scale then
                            AtRect
                                ( (pageX - previewMarginX - width * scale) // scale
                                , pageY // scale
                                )
                                leftMouseButton
                        else Somewhere
        )


mouseToMessage : Model -> MouseAt -> Msg
mouseToMessage model mouseAt =
    case mouseAt of
        AtGrid pos mouseDown shiftKey ->
            if shiftKey == ShiftKeyDown && mouseDown == LMBDown then
                PackOneCloseTo pos model.nextRect
            else if mouseDown == LMBDown then
                PackOneAt pos model.nextRect
            else
                AddGridPreview pos model.nextRect
        AtRect ( width, height ) mouseDown ->
            if mouseDown == LMBDown then
                SetNextRect
                    { width = width
                    , height = height
                    , color = model.nextRect.color
                    }
            else
                AddRectPreview
                    { width = width
                    , height = height
                    , color = model.nextRect.color
                    }
        Somewhere -> NoOp


labelForDistribution : SP.Distribution -> String
labelForDistribution d =
    case d of
        SP.Up -> "↑"
        SP.Down -> "↓"
        SP.Right -> "→"
        SP.Left -> "←"
