port module Example.Tiler.Logic exposing (..)


import Dict exposing (Dict)

import Html exposing (Html)
import Html.Attributes as HA

import WithTron.ValueAt exposing (ValueAt)

import Example.Tiler.Product as Product exposing (Product)


type Msg
    = NoOp
    | WaitingForTileset Tileset
    | TilesetReady Tileset
    | TilesetFailedToLoad Tileset


type TilesetStatus
    = Waiting
    | FailedToLoad
    | Ready


type alias Tileset = String


type alias Model =
    Dict Tileset TilesetStatus


init : flags -> ValueAt -> ( Model, Cmd Msg )
init _ _ =
    ( Dict.empty
    , Cmd.none
    )


update : Msg -> ValueAt -> Model -> ( Model, Cmd Msg )
update msg _ tilesets =
    ( case msg of
        WaitingForTileset tileset ->
            tilesets
                |> Dict.insert tileset Waiting

        TilesetReady tileset ->
            tilesets
                |> Dict.insert tileset Ready

        TilesetFailedToLoad tileset ->
            tilesets
                |> Dict.insert tileset FailedToLoad
        NoOp -> tilesets
    , Cmd.none
    )


view : ValueAt -> Model -> Html Msg
view _ tilesets =
    let
        viewTileset ( tileset, status ) =
            Html.span []
                [ Html.text <| statusMark status ++ " " ++ tileset
                ]
    in
    Html.div []
        <| List.map
            (Html.div [ HA.style "margin" "5px" ]
                << List.singleton
                << viewTileset
            )
        <| Dict.toList
        <| tilesets


statusMark : TilesetStatus -> String
statusMark status =
    case status of
        Waiting -> "⌛"
        Ready -> "✓"
        FailedToLoad -> "✘"


subscriptions : ValueAt -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ tilesetReady TilesetReady
        , waitingForTileset WaitingForTileset
        , tilesetFailedToLoad TilesetFailedToLoad
        ]



port tilesetReady : (String -> msg) -> Sub msg


port waitingForTileset : (String -> msg) -> Sub msg


port tilesetFailedToLoad : (String -> msg) -> Sub msg
