module BinPackDemo exposing (..)


import Browser
import Html exposing (Html, button, div, text, input, ul, li)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Task

import BinPack exposing (BinPack)
import BinPack as BinPack exposing (..)


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


type alias Rect =
    { width : Float
    , height : Float
    , color : Color
    }


type RenderMode
    = OnlyRects
    | RectsAndFreeSpace


type alias Model =
    { mode : RenderMode
    , binPack : BinPack Color
    , nextRect : Maybe Rect
    }


init : ( Model, Cmd Msg )
init =
    (
        { mode = OnlyRects
        , binPack = container 0 0
        , nextRect = Nothing
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
  | SetNextRectWidth Float
  | SetNextRectHeight Float
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
           | binPack = container 300 300
           }
       , Random.generate
            PackAll
            random
       )

    PackAll rects ->
        (
            { model
            | binPack =
                rects
                    |> List.map rectToTuple
                    |> List.foldl BinPack.pack1 model.binPack
            }

        , Cmd.none
        )

    PackOne rect ->
        (
            { model
            | binPack =
                model.binPack
                    |> BinPack.pack1 (rectToTuple rect)
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

    Clear ->
        (
            { model
            | binPack = container 300 300
            }
        , Cmd.none
        )

    Error rect ->
      ( model, Cmd.none )



rectToTuple { width, height, color }
    = ( { width = width , height = height }, color )


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
        viewItem (color, bounds)
            = rect color bounds "black"
        viewBP ( bp, bounds )
            = case bp of
                Node _ _ item ->
                    viewItem ( item, bounds )
                Free _ ->
                    rect "yellow" bounds "red"
    in
        div
            []
            [ case model.mode of
                OnlyRects ->
                    svg [ S.width "300", S.height "300" ]
                        <| List.map viewItem
                        <| BinPack.unpack model.binPack
                RectsAndFreeSpace ->
                    svg [ S.width "300", S.height "300" ]
                        <| List.map viewBP
                        <| BinPack.unpack1 model.binPack
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
                    [ H.type_ "button", onClick <| ChangeMode OnlyRects, H.value "Only Rectangles" ]
                    [ Html.text "Only Rectangles" ]
                , input
                    [ H.type_ "button", onClick <| ChangeMode RectsAndFreeSpace, H.value "Rectangles and Free Space" ]
                    [ Html.text "Rectangles and Free Space" ]
                ]
            , div
                [ ]
                [ input
                    [ H.type_ "number"
                    , onInput (String.toFloat >> Maybe.map SetNextRectWidth >> Maybe.withDefault NoOp)
                    , H.placeholder "20" ]
                    [ ]
                , Html.text "x"
                , input
                    [ H.type_ "number"
                    , onInput (String.toFloat >> Maybe.map SetNextRectHeight >> Maybe.withDefault NoOp)
                    , H.placeholder "20" ]
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
        (Random.float 0 70)
        (Random.float 0 70)
        randomColor


random : Random.Generator (List Rect)
random =
    Random.int 10 200
      |> Random.andThen
          (\len -> Random.list len randomRect)

