port module Example.Tiler.Logic exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Example.Tiler.Product exposing (Product)

import Tron.Builder as Tron exposing (Face)
import Tron.Style.Theme as Tron
import WithTron.ValueAt exposing (ValueAt)


type alias TileCount = Int


type alias SizeInfo =
     { sizeInTiles : ( Int, Int )
     , logoPosition : ( Int, Int )
     , titlePosition : ( Int, Int )
     , textBlockSize : Float
     , screenSize : ( Int, Int )
     }


type Msg
    = NoOp
    | WaitingForTileset Tileset
    | TilesetReady Tileset TileCount
    | TilesetFailedToLoad Tileset
    | SetPreselectedTileset Tileset
    | Resize SizeInfo


type TilesetStatus
    = Waiting
    | FailedToLoad
    | Ready TileCount


type alias Tileset =
    String


type alias Tilesets =
    Dict Tileset TilesetStatus


type alias Model =
    { tilesets : Tilesets
    , preselectedTileset : Maybe Tileset
    , sizeInTiles : ( Int, Int )
    , logoPosition : ( Int, Int )
    , titlePosition : ( Int, Int )
    , textBlockSize : Float
    , screenSize : ( Int, Int )
    }


init : flags -> ValueAt -> ( Model, Cmd Msg )
init _ _ =
    (
        { tilesets = Dict.empty
        , preselectedTileset = Nothing
        , sizeInTiles = ( 0, 0 )
        , logoPosition = ( 0, 0 )
        , titlePosition = ( 0, 0 )
        , textBlockSize = 0
        , screenSize = ( 0, 0 )
        }
    , Cmd.none
    )


update : Msg -> ValueAt -> Model -> ( Model, Cmd Msg )
update msg _ model =
    ( case msg of
        WaitingForTileset tileset ->
            { model
            | tilesets =
                model.tilesets
                    |> Dict.insert tileset Waiting
            }

        TilesetReady tileset count ->
            { model
            | tilesets =
                model.tilesets
                    |> Dict.insert tileset (Ready count)
            }

        TilesetFailedToLoad tileset ->
            { model
            | tilesets =
                model.tilesets
                    |> Dict.insert tileset FailedToLoad
            }

        SetPreselectedTileset tileset ->
            { model
            | preselectedTileset = Just tileset
            }

        Resize sizeInfo ->
            { model
            | sizeInTiles = sizeInfo.sizeInTiles
            , logoPosition = sizeInfo.logoPosition
            , titlePosition = sizeInfo.titlePosition
            , textBlockSize = sizeInfo.textBlockSize
            , screenSize = sizeInfo.screenSize
            }

        NoOp ->
            model
    , Cmd.none
    )


view : ValueAt -> Model -> Html Msg
view _ tilesets =
    Html.div [ HA.style "display" "none" ] []



{-
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
-}


statusIcon : TilesetStatus -> Face
statusIcon status =
    Tron.themedIconAt
        (\theme ->
            [ "tron"
            , "icons"
            , case theme of
                Tron.Dark -> "light-stroke"
                Tron.Light -> "dark-stroke"
            , (case status of
                Waiting ->
                    "tick"

                Ready _ ->
                    "loaded"

                FailedToLoad ->
                    "error"
            ) ++ ".svg"
            ]
        )


subscriptions : ValueAt -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ tilesetReady (\(set, count) -> TilesetReady set count)
        , waitingForTileset WaitingForTileset
        , tilesetFailedToLoad TilesetFailedToLoad
        , setPreselectedTileset SetPreselectedTileset
        , screenSizeChanged Resize
        ]


port tilesetReady : ((Tileset, TileCount) -> msg) -> Sub msg


port waitingForTileset : (Tileset -> msg) -> Sub msg


port tilesetFailedToLoad : (Tileset -> msg) -> Sub msg


port setPreselectedTileset : (Tileset -> msg) -> Sub msg


port screenSizeChanged : ( SizeInfo -> msg) -> Sub msg