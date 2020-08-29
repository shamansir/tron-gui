module BinPackDemo exposing (..)


import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onClick)
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


type alias Model =
    BinPack Color


init : ( Model, Cmd Msg )
init =
    ( container 0 0
    , Task.succeed ()
        |> Task.perform (always Randomize)
    )


-- UPDATE


type Msg
  = Randomize
  | Pack (List Rect)
  | Error Rect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Randomize ->
       ( container 300 300
       , Random.generate
            Pack
            random
       )

    Pack rects ->
        let
            rectToTuple { width, height, color }
                = ( { width = width , height = height }, color )
        in
            ( rects
                |> List.map rectToTuple
                |> List.foldl BinPack.pack1 model
            , Cmd.none
            )

    Error rect ->
      ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewItem (color, bounds)
            = Svg.rect
                [ S.x <| String.fromFloat bounds.x
                , S.y <| String.fromFloat bounds.y
                , S.width <| String.fromFloat bounds.width
                , S.height <| String.fromFloat bounds.height
                , S.fill color
                , S.strokeWidth "1"
                , S.stroke "black"
                ]
                []
    in
        div
            []
            [ input [ H.type_ "button", onClick Randomize, H.value "Random" ] [ Html.text "Random" ]
            , svg [ S.width "300", S.height "300" ]
                 <| List.map viewItem
                 <| BinPack.unpack model
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
    Random.int 10 60
      |> Random.andThen
          (\len -> Random.list len randomRect)

