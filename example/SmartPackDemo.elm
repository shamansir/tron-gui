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


defaultSize : Size Cells
defaultSize = Size ( 20, 20 )


defaultDistribution : SP.Distribution
defaultDistribution = SP.Right


type RenderMode
    = Rects
    | Grid


type alias Model =
    { mode : RenderMode
    , smartPack : SmartPack Color
    , nextRect : Rect
    , nextPos : Maybe Pos
    , distribution : Maybe SP.Distribution
    }


init : ( Model, Cmd Msg )
init =
    (
        { mode = Grid
        , smartPack = SP.container defaultSize
        , nextRect = defaultRect
        , nextPos = Nothing
        , distribution = Nothing
        }
    , Task.succeed ()
        |> Task.perform (always Randomize)
    )


-- UPDATE


type Msg
  = NoOp
  | ChangeMode RenderMode
  | Randomize
  | PackAll (List Rect)
  | PackOne Rect
  | PackOneAt ( Int, Int ) Rect
  | SetNextRectWidth Int
  | SetNextRectHeight Int
  | SetNextRectX Int
  | SetNextRectY Int
  | SetGridWidth Int
  | SetGridHeight Int
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
                                (model.distribution
                                    |> Maybe.withDefault defaultDistribution
                                )
                                (Size ( width, height))
                                color

                        )
                        model.smartPack
            }
        , Cmd.none
        )

    PackOne { width, height, color } ->
        (
            { model
            | smartPack =

                model.smartPack
                    |> SP.carelessPack
                        (model.distribution
                            |> Maybe.withDefault defaultDistribution
                        )
                        (Size ( width, height ))
                        color

            }
        , Cmd.none
        )

    PackOneAt (x, y) { width, height, color } ->
        (
            { model
            | smartPack =
                model.smartPack
                    |> SP.carelessPackAt (x, y) (Size ( width, height )) color
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


view : Model -> Html Msg
view model =
    let
        rect color bounds =
            Svg.rect
                [ S.x <| String.fromFloat <| bounds.x * scale
                , S.y <| String.fromFloat <| bounds.y * scale
                , S.width <| String.fromFloat <| bounds.width * scale
                , S.height <| String.fromFloat <| bounds.height * scale
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
        rowsToCells rows =
            rows
                |> Array.indexedMap
                    (\y row ->
                        row
                            |> Array.indexedMap
                                (\x ( v, maybeColor ) ->
                                    { x = toFloat x
                                    , y = toFloat y
                                    , weight = v
                                    , value = maybeColor |> Maybe.withDefault "none"
                                    }
                                )
                    )
                |> Array.foldl Array.append Array.empty
    in
        div
            []
            [ case ( model.mode, SP.dimensions model.smartPack ) of
                ( Rects, Size ( width, height ) ) ->
                    svg [ S.width <| String.fromInt <| width * scale
                        , S.height <| String.fromInt <| height * scale
                        ]
                        <| List.map viewItem
                        <| SP.toList model.smartPack
                ( Grid, Size ( width, height ) ) ->
                    svg [ S.width <| String.fromInt <| width * scale
                        , S.height <| String.fromInt <| height * scale
                        ]
                        <| List.concat
                        <| Matrix.toList
                        <| Matrix.indexedMap viewGridCell
                        <| SP.toMatrix model.smartPack
            ,
                case model.nextRect of
                    { width, height, color } ->
                        svg [ S.width <| String.fromInt <| width * scale
                            , S.height <| String.fromInt <| height * scale
                            , S.style "position: absolute; top: 20px; margin-left: 50px;"
                            ]
                            [ rect
                                color
                                { x = 0, y = 0, width = toFloat width, height = toFloat height }
                            ]
            , div
                []
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
                [ ]
                [ input
                    [ H.type_ "number"
                    , onInput (String.toInt >> Maybe.map SetNextRectWidth >> Maybe.withDefault NoOp)
                    , H.placeholder <| String.fromInt <| model.nextRect.width
                    , H.value <| String.fromInt <| model.nextRect.width
                    ]
                    [ ]
                , Html.text "x"
                , input
                    [ H.type_ "number"
                    , onInput (String.toInt >> Maybe.map SetNextRectHeight >> Maybe.withDefault NoOp)
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
    Browser.Events.onMouseMove
        (whereIsMouse model
            |> D.map (Debug.log "mm")
            |> D.map (always NoOp)
        )

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


type MouseAt
    = AtGrid ( Int, Int ) Bool
    | AtRect ( Int, Int ) Bool
    | Somewhere


whereIsMouse : Model -> D.Decoder MouseAt
whereIsMouse model =
    D.map3
        (\pageX pageY buttons ->
            ( ( pageX, pageY ), buttons == 1 )
        )
        (D.field "pageX" D.float |> D.map floor)
        (D.field "pageY" D.float |> D.map floor)
        (D.field "buttons" D.int)
    |> D.map
        (\((pageX, pageY), leftButtonDown) ->
            case model.smartPack |> SP.dimensions of
                Size ( width, height ) ->
                    if pageX <= width * scale && pageY <= height * scale then
                        AtGrid
                            ( pageX // scale, pageY // scale )
                            leftButtonDown
                    else
                        if pageX > (width * scale + 20) && pageY <= height * scale then
                            AtRect
                                ( (pageX - 20 - width * scale) // scale
                                , pageY // scale
                                )
                                leftButtonDown
                        else Somewhere

        )

