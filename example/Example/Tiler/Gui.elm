module Example.Tiler.Gui exposing (gui)


import Dict
import Color exposing (Color)
import Tron exposing (Tron)
import Tron.Builder.Unit as Tron
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape as CellShape exposing (..)
import Tron.Style.Theme as Theme


import Example.Tiler.Product as Product exposing (Product)
import Example.Tiler.Logic as Logic exposing (Model, Tileset, Tilesets, TilesetStatus, statusIcon)

import WithTron.ValueAt as V exposing (ValueAt)

import Tron.Control.Value as Proxy exposing (Value)


type FullscreenStatus
    = Fullscreen
    | CustomSize


type GradientStatus
    = Gradient
    | Discrete


gui : ValueAt -> Model -> Tron ()
gui valueAt model =
    Tron.root
        [ ( "Color Scheme",
                colorScheme
                    (loadProduct valueAt)
                    |> Tron.face (icon "chromatic")
          )
        , ( "Sizes",
                sizes
                    (isFullscreenEnabled valueAt)
                    |> Tron.face (icon "size")
          )
        , ( "Tile",
                tile
                    (Dict.toList model.tilesets)
                    (getPreselectedTileset model)
                |> Tron.face (icon "tile")
          )
        , ( "Randomness",
                randomness
                    (isGradientEnabled valueAt)
                    (getTileCount model.tilesets valueAt)
                    |> Tron.face (icon "settings") )
        , ( "Title", title model.textBlockSize model.titlePosition model.screenSize |> Tron.face (icon "text") )
        , ( "Logo", logo model.sizeInTiles model.logoPosition |> Tron.face (icon "text") )
        , ( "Animation", animation |> Tron.face (icon "animation") )
        , ( "Click action", clickAction |> Tron.face (icon "cursor"))
        , ( "Click opacity", clickOpacity <| loadActionType valueAt )
        , ( "Shuffle tiles", Tron.button |> Tron.face (icon "shuffle"))
        , ( "Refresh", Tron.button |> Tron.face (icon "update"))
        , ( "Make URL", Tron.button |> Tron.face (icon "link" ))
        , ( "Export", Tron.button |> Tron.face (icon "save"))
        , ( "Upload tiles", Tron.button |> Tron.face (icon "export"))
        , ( "TestValue", Tron.int { min = 0, max = 255, step = 1 } <| Tuple.second model.screenSize )
        ]


loadProduct : ValueAt -> Product
loadProduct =
    V.ask
        (V.choiceOf
            (Product.all |> List.filter Product.hasIcon)
            [ "Color Scheme", "Product" ]
        )
        >> Maybe.withDefault Product.default


loadActionType : ValueAt -> ActionType
loadActionType =
    V.ask
        (V.choiceOf
            actionTypesOrder
            [ "Click action" ]
        )
        >> Maybe.withDefault defaultActionType


isFullscreenEnabled : ValueAt -> FullscreenStatus
isFullscreenEnabled =
    V.ask (V.toggle [ "Sizes", "Fullscreen" ])
        >> Maybe.map Proxy.toggleToBool
        >> Maybe.map (\v -> if v then Fullscreen else CustomSize)
        >> Maybe.withDefault Fullscreen


isGradientEnabled : ValueAt -> GradientStatus
isGradientEnabled =
    V.ask (V.toggle [ "Randomness", "Gradient" ])
        >> Maybe.map Proxy.toggleToBool
        >> Maybe.map (\v -> if v then Gradient else Discrete)
        >> Maybe.withDefault Discrete


getPreselectedTileset : Model -> Tileset
getPreselectedTileset { preselectedTileset, tilesets } =
    preselectedTileset
        |> Maybe.withDefault
                (Dict.toList tilesets
                    |> List.head
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault "None"
                )


getTileCount : Tilesets -> ValueAt -> Logic.TileCount
getTileCount tilesets =
    loadTileSet tilesets
        >> Maybe.map Tuple.second
        >> Maybe.map
            (\status ->
                case status of
                    Logic.Ready tileCount -> tileCount
                    _ -> 0
            )
        >> Maybe.withDefault 0


loadTileSet : Tilesets -> ValueAt -> Maybe ( Tileset, TilesetStatus )
loadTileSet tilesets =
    V.ask
        (V.choiceOf
            (Dict.toList tilesets)
            [ "Tile", "Tileset" ]
        )


colorScheme : Product -> Tron ()
colorScheme curProduct =
    Tron.nest
        [ ( "Product", products )
        , ( "Base color", baseColor )
        , ( "BG color", bgColor <| Product.getPalette curProduct )
        , ( "Opacity", Tron.int { min = 0, max = 255, step = 1 } 0 )
        ]
        |> Tron.shape (cols 2)


products : Tron ()
products =
    Tron.choiceBy
        (Product.all
            |> List.filter Product.hasIcon
            |> Tron.buttons
            |> List.map (Tron.with (Tron.face << productIcon))
            |> Tron.addLabels Product.getName
        )
        Product.default
        Product.compare
    |> Tron.shape (rows 3)
--    |> Tron.cells (CellShape.twiceByHalf)


clickOpacity : ActionType -> Tron ()
clickOpacity currentAction =
    case currentAction of
        ChangeOpacity ->
            Tron.float { min = 0, max = 1, step = 0.1 } 0
        _ -> Tron.none


colorToString : Color -> String
colorToString = always "white"


productIcon : Product -> Tron.Face
productIcon product =
    Tron.iconAt
        [ "assets"
        , "tiler"
        , "product-logos"
        , (product |> Product.iconName |> Maybe.withDefault "none") ++ ".svg"
        ]

icon : String -> Tron.Face
icon name =
    Tron.themedIconAt
        (\theme ->
            [ "assets"
            , "tiler"
            , case theme of
                Theme.Dark -> "light-stroke"
                Theme.Light -> "dark-stroke"
            , name ++ ".svg"
            ]
        )


sizes : FullscreenStatus -> Tron ()
sizes fullscreenStatus =
     Tron.nest
        (
            [ ( "Cell", Tron.int { min = 60, max = 200, step = 1 } 100 )
            , ( "Shape", Tron.float { min = 0.1, max = 5, step = 0.1 } 1 )
            ] ++
                case fullscreenStatus of
                    Fullscreen ->
                        [ ( "Fullscreen", Tron.toggle True ) ]
                    CustomSize ->
                        [ ( "Fullscreen", Tron.toggle True )
                        -- FIXME: should take values from screen size
                        , ( "Columns", Tron.int { min = 1, max = 100, step = 1 } 13 )
                        , ( "Rows", Tron.int { min = 1, max = 100, step = 1 } 8 )
                        ]
        )
        |> Tron.shape (cols 3)


randomness : GradientStatus -> Logic.TileCount -> Tron ()
randomness gradientStatus tileCount =
     Tron.nest
        [ ( "Diversity", Tron.int { min = 1, max = tileCount, step = 1 } tileCount )
        , ( "Tones", Tron.int { min = 3, max = 17, step = 2 } 3 )
        , ( "Recolor", Tron.button |> Tron.face (icon "update"))
        , ( "Gradient", Tron.toggle False )
        , ( "Gradient size",
                case gradientStatus of
                    Gradient ->
                        Tron.float { min = 4, max = 200, step = 1 } 100
                    Discrete ->
                        Tron.none
          )
        , ( "Smooth color",
                case gradientStatus of
                    Gradient ->
                        Tron.toggle False
                    Discrete ->
                        Tron.none
          )
        ]
        |> Tron.shape (cols 3)


tile : List ( Tileset, TilesetStatus ) -> Tileset -> Tron ()
tile tilesets defaultTileset =
    Tron.nest
        [ ( "Tileset", tileset tilesets defaultTileset )
        , ( "Stroke weight", Tron.int { min = 0, max = 10, step = 1 } 0 )
        , ( "Fill α", Tron.int { min = 0, max = 255, step = 1 } 178 )
        , ( "Stroke α", Tron.int { min = 0, max = 255, step = 1 } 255 )
        ]
        |> Tron.shape (cols 2)


tileset : List ( Tileset, TilesetStatus ) -> Tileset -> Tron ()
tileset tilesets defaultTileset =
    Tron.choice
        (tilesets
            |> Tron.buttons
            --|> List.map (Tron.with (Tron.face << statusIcon << Tuple.second))
            |> Tron.addLabels Tuple.first
            |> Tron.mapSet Tuple.first
        )
        defaultTileset
    |> Tron.cells CellShape.twiceByHalf
    |> Tron.shape (rows 5)
    -- |> Tron.shape (by 1 5)


bgColor : Product.Palette -> Tron ()
bgColor palette =
    Tron.palette
        [ ( "front", palette |> Product.getPaletteColor Product.ColorIII )
        , ( "middle", palette |> Product.getPaletteColor Product.ColorII )
        , ( "rear", palette |>  Product.getPaletteColor Product.ColorI )
        ]
        Color.white
    |> Tron.cells CellShape.single
    -- |> Tron.cells CellShape.twiceByHalf


baseColor : Tron ()
baseColor =
    Tron.palette
        [ ( "default", Color.gray )
        , ( "black", Color.black )
        , ( "white", Color.white )
        ]
        Color.white
    |> Tron.cells CellShape.single
    -- |> Tron.cells CellShape.twiceByHalf


title : Float -> (Int, Int) -> (Int, Int) -> Tron ()
title textCoef (titleX, titleY) (screenX, screenY) =
    Tron.nest
        [ ( "Show", Tron.toggle True)
        , ( "X",
                Tron.int
                    { min = 0, max = screenX, step = 1 }
                     <| min screenX titleX
          )
        , ( "Y",
                Tron.int
                    { min = 0, max = screenY, step = 1 }
                    <| min screenY titleY
          )
        ,( "Scale",
                 Tron.float
                     { min = 0.1, max = 5, step = 0.1 }
                     <| textCoef
         )

        ]
        |> Tron.shape (cols 3)


logo : (Int, Int) -> (Int, Int) -> Tron ()
logo ( amountX, amountY ) ( logoX, logoY ) =
    Tron.nest
        [ ( "Show", Tron.toggle True)
        , ( "X",
                Tron.int
                    { min = 0, max = amountX - 1, step = 1 }
                     <| min (amountX - 1) logoX
          )
        , ( "Y",
                Tron.int
                    { min = 0, max = amountY - 1, step = 1 }
                    <| min (amountY - 1) logoY )
        --, ( "Font size", Tron.int { min = 0, max = 72, step = 1 } 16 )
        --, ( "Opacity", Tron.int { min = 0, max = 255, step = 1 } 255 )
        --, ( "Position",
        --        Tron.xy
        --            ( { min = 0, max = 20, step = 1 }
        --            , { min = 0, max = 20, step = 1 }
        --            )
        --            (14, 18) )
        --, ( "Color",  Tron.color <| Color.rgb255 255 194 0)
        ]
        |> Tron.shape (cols 3)


type ActionType
    = Rotate
    | ModifyTile
    | ChangeOpacity


actionTypesOrder = [ Rotate, ModifyTile, ChangeOpacity ]


defaultActionType = ModifyTile


actionTypeToString : ActionType -> String
actionTypeToString actionType_ =
    case actionType_ of
        Rotate -> "Rotate"
        ModifyTile -> "Modify tile"
        ChangeOpacity -> "Change opacity"


clickAction : Tron ()
clickAction =
    Tron.labels
        actionTypeToString
        actionTypesOrder
        defaultActionType


animation : Tron ()
animation =
    Tron.nest
        [ ( "Animate", Tron.toggle False )
        , ( "Duration", Tron.float { min = 0, max = 4, step = 0.1 } 3 )
        , ( "Delay", Tron.float { min = 0, max = 4, step = 0.1 } 3 )
        ]
        |> Tron.shape (cols 2)
