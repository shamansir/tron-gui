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
    { tilesets : Dict Tileset TilesetStatus
    , lastProduct : Product
    }


tilesets =
    [ "Foo", "Bar", "Test", "AAAA1", "AAABB2", "FooFoo", "BarBar", "Lalalal", "Cooockoo"
    , "Foo1", "Bar1", "Test1", "AAAA2", "AAABB2", "FooBar", "BarFooooo", "LalalalKek", "Cuuuckuu"
    ]
        -- FIXME: we're required to know the size of the list beforehand or else
        -- the `Choice` control won't be properly created


init : flags -> ValueAt -> ( Model, Cmd Msg )
init _ _ =
    (
        { tilesets =
            -- FIXME: chould be `Dict.empty` at start
            Dict.fromList <| List.map (\name -> (name, Waiting)) tilesets
        , lastProduct = Product.default
        }
    , Cmd.none
    )


update : Msg -> ValueAt -> Model -> ( Model, Cmd Msg )
update msg _ model =
    ( case msg of
        WaitingForTileset tileset ->
            { model
            | tilesets =
                (model.tilesets
                    |> Dict.insert tileset Waiting )
            }

        TilesetReady tileset ->
            { model
            | tilesets =
                (model.tilesets
                    |> Dict.insert tileset Ready )
            }

        TilesetFailedToLoad tileset ->
            { model
            | tilesets =
                (model.tilesets
                    |> Dict.insert tileset FailedToLoad )
            }
        NoOp -> model
    , Cmd.none
    )


view : ValueAt -> Model -> Html Msg
view _ model =
    let
        viewTileset ( tileset, status ) =
            Html.span []
                [ (case status of
                    Waiting -> "⌛"
                    Ready -> "✓"
                    FailedToLoad -> "✘")
                    ++ " " ++ tileset
                |> Html.text
                ]
    in
    Html.div []
        <| List.map
            (Html.div [ HA.style "margin" "5px" ]
                << List.singleton
                << viewTileset
            )
        <| Dict.toList
        <| model.tilesets


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
