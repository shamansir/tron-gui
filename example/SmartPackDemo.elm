module SmartPackDemo exposing (..)


import Browser
import Html exposing (Html, button, div, text, input, ul, li)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Task

import Size exposing (..)
import SmartPack exposing (SmartPack)
import SmartPack as SP


-- MAIN


main : Program () Model Msg
main =
    Browser.element
      { init = always init
      , update = update
      , view = view
      , subscriptions = always Sub.none
      }


-- MODEL


type alias Color = String


type alias Pos = ( Float, Float )


type alias Rect =
    { width : Float
    , height : Float
    , color : Color
    }


type RenderMode
    = Rects
    | Matrix
    | RectsAndMatrix


type alias Model =
    { mode : RenderMode
    , smartPack : SmartPack Color
    , nextRect : Maybe Rect
    , nextPos : Maybe Pos
    }


init : ( Model, Cmd Msg )
init =
    (
        { mode = Rects
        , smartPack = SP.container <| Size ( 0, 0 )
        , nextRect = Nothing
        , nextPos = Nothing
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
  | PackOneAt ( Float, Float ) Rect
  | SetNextRectWidth Float
  | SetNextRectHeight Float
  | SetNextRectX Float
  | SetNextRectY Float
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
           | smartPack = SP.container <| Size ( 20, 20 )
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
                            SP.carelessPack (SizeF ( width, height)) color
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
                    |> SP.carelessPack (SizeF ( width, height )) color
            }
        , Cmd.none
        )

    PackOneAt (x, y) { width, height, color } ->
        (
            { model
            | smartPack =
                model.smartPack
                    |> SP.carelessPackAt (x, y) (SizeF ( width, height )) color
            }
        , Cmd.none
        )

    SetNextRectWidth width ->
        (
            { model
            | nextRect =
                case model.nextRect of
                    Just r ->
                        Just
                            { r
                            | width = width
                            }
                    Nothing ->
                        Just
                            { width = width
                            , height = 0
                            , color = "#ffffff"
                            }
            }
        , Cmd.none
        )

    SetNextRectHeight height ->
        (
            { model
            | nextRect =
                case model.nextRect of
                    Just r ->
                        Just
                            { r
                            | height = height
                            }
                    Nothing ->
                        Just
                            { height = height
                            , width = 0
                            , color = "#ffffff"
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

    Clear ->
        (
            { model
            | smartPack = SP.container <| Size ( 300, 300 )
            }
        , Cmd.none
        )

    Error rect ->
      ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        rect color bounds stroke =
            Svg.rect
                [ S.x <| String.fromFloat bounds.x
                , S.y <| String.fromFloat bounds.y
                , S.width <| String.fromFloat bounds.width
                , S.height <| String.fromFloat bounds.height
                , S.fill color
                , S.strokeWidth "1"
                , S.stroke stroke
                ]
                []
        viewItem (bounds, color)
            = rect color bounds "black"
    in
        div
            []
            [ case model.mode of
                Rects ->
                    svg [ S.width "300", S.height "300" ]
                        <| List.map viewItem
                        <| SP.asList model.smartPack
                Matrix ->
                    svg [ S.width "300", S.height "300" ]
                        <| List.map viewItem
                        <| SP.asList model.smartPack
                RectsAndMatrix ->
                    svg [ S.width "300", S.height "300" ]
                        <| List.map viewItem
                        <| SP.asList model.smartPack
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
                    [ H.type_ "button", onClick <| ChangeMode Matrix, H.value "Matrix" ]
                    [ Html.text "Matrix" ]
                , input
                    [ H.type_ "button", onClick <| ChangeMode Matrix, H.value "Rectangles & Matrix" ]
                    [ Html.text "Rectangles & Matrix" ]
                ]
            , div
                [ ]
                [ input
                    [ H.type_ "number"
                    , onInput (String.toFloat >> Maybe.map SetNextRectWidth >> Maybe.withDefault NoOp)
                    , H.placeholder "4" ]
                    [ ]
                , Html.text "x"
                , input
                    [ H.type_ "number"
                    , onInput (String.toFloat >> Maybe.map SetNextRectHeight >> Maybe.withDefault NoOp)
                    , H.placeholder "2" ]
                    [ ]
                , input
                    [ H.type_ "button"
                    , onClick (model.nextRect |> Maybe.map PackOne |> Maybe.withDefault NoOp)
                    , H.value "Add Rect"
                    ]
                    [ Html.text "Add Rect" ]
                ]
            ]


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
        (Random.float 1 4)
        (Random.float 1 4)
        randomColor


random : Random.Generator (List Rect)
random =
    Random.int 2 15
      |> Random.andThen
          (\len -> Random.list len randomRect)

