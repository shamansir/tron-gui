module Example.Tiler.Gui exposing (gui)


import Dict
import Color exposing (Color)
import Tron exposing (Tron)
import Tron.Tree exposing (Tree)
import Tron.Tree.Build.Unit as Gui
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape as CellShape exposing (..)
import Tron.Style.Theme as Theme


import Example.Tiler.Product as Product exposing (Product)
import Example.Tiler.Logic as Logic exposing (Model, Tileset, Tilesets, TilesetStatus, statusIcon)

import WithTron.ValueAt as V

import Tron.Control.Value as Proxy exposing (Value)


type FullscreenStatus
    = Fullscreen
    | CustomSize


type GradientStatus
    = Gradient
    | Discrete


gui : Tree () -> Model -> Tree ()
gui valueAt model =
    Gui.root
        [ ( "Color Scheme",
                colorScheme
                    (loadProduct valueAt)
                    |> Gui.face (icon "chromatic")
          )
        , ( "Sizes",
                sizes
                    (isFullscreenEnabled valueAt)
                    |> Gui.face (icon "size")
          )
        , ( "Tile",
                tile
                    (Dict.toList model.tilesets)
                    (getPreselectedTileset model)
                |> Gui.face (icon "tile")
          )
        , ( "Randomness",
                randomness
                    (isGradientEnabled valueAt)
                    (getTileCount model.tilesets valueAt)
                    |> Gui.face (icon "settings") )
        , ( "Title", title model.textBlockSize model.titlePosition model.screenSize |> Gui.face (icon "text") )
        , ( "Logo", logo model.sizeInTiles model.logoPosition |> Gui.face (icon "text") )
        , ( "Animation", animation |> Gui.face (icon "animation") )
        , ( "Click action", clickAction |> Gui.face (icon "cursor"))
        , ( "Click opacity", clickOpacity <| loadActionType valueAt )
        , ( "Shuffle tiles", Gui.button |> Gui.face (icon "shuffle"))
        , ( "Refresh", Gui.button |> Gui.face (icon "update"))
        , ( "Make URL", Gui.button |> Gui.face (icon "link" ))
        , ( "Export", Gui.button |> Gui.face (icon "save"))
        , ( "Upload tiles", Gui.button |> Gui.face (icon "export"))
        , ( "TestValue",
                Gui.int
                    { min = 0, max = 4000, step = 1 }
                    (Tuple.second model.screenSize)
                |> Gui.live)
        ]


loadProduct : Tree () -> Product
loadProduct =
    V.ask
        (V.choiceOf
            (Product.all |> List.filter Product.hasIcon)
            [ "Color Scheme", "Product" ]
        )
        >> Maybe.withDefault Product.default


loadActionType : Tree () -> ActionType
loadActionType =
    V.ask
        (V.choiceOf
            actionTypesOrder
            [ "Click action" ]
        )
        >> Maybe.withDefault defaultActionType


isFullscreenEnabled : Tree () -> FullscreenStatus
isFullscreenEnabled =
    V.ask (V.toggle [ "Sizes", "Fullscreen" ])
        >> Maybe.map Proxy.toggleToBool
        >> Maybe.map (\v -> if v then Fullscreen else CustomSize)
        >> Maybe.withDefault Fullscreen


isGradientEnabled : Tree () -> GradientStatus
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


getTileCount : Tilesets -> Tree () -> Logic.TileCount
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


loadTileSet : Tilesets -> Tree () -> Maybe ( Tileset, TilesetStatus )
loadTileSet tilesets =
    V.ask
        (V.choiceOf
            (Dict.toList tilesets)
            [ "Tile", "Tileset" ]
        )


colorScheme : Product -> Tree ()
colorScheme curProduct =
    Gui.nest
        [ ( "Product", products )
        , ( "Base color", baseColor )
        , ( "BG color", bgColor <| Product.getPalette curProduct )
        , ( "Opacity", Gui.int { min = 0, max = 255, step = 1 } 0 )
        ]
        |> Gui.shape (cols 2)
        |> Gui.expand


products : Tree ()
products =
    Gui.choiceBy
        (Product.all
            |> List.filter Product.hasIcon
            |> Gui.buttons
            |> List.map (Gui.with (Gui.face << productIcon))
            |> Gui.addLabels Product.getName
        )
        Product.default
        Product.compare
    |> Gui.shape (rows 3)
    |> Gui.expand
    --|> Gui.toKnob
    --|> Gui.toSwitch
    --    |> Gui.cells (CellShape.twiceByHalf)


clickOpacity : ActionType -> Tree ()
clickOpacity currentAction =
    case currentAction of
        ChangeOpacity ->
            Gui.float { min = 0, max = 1, step = 0.1 } 0
        _ -> Gui.none


colorToString : Color -> String
colorToString = always "white"


productIcon : Product -> Tron.Face
productIcon product =
    Gui.iconAt
        [ "assets"
        , "tiler"
        , "product-logos"
        , (product |> Product.iconName |> Maybe.withDefault "none") ++ ".svg"
        ]

icon : String -> Tron.Face
icon name =
    Gui.themedIconAt
        (\theme ->
            [ "assets"
            , "tiler"
            , case theme of
                Theme.Dark -> "light-stroke"
                Theme.Light -> "dark-stroke"
            , name ++ ".svg"
            ]
        )


sizes : FullscreenStatus -> Tree ()
sizes fullscreenStatus =
     Gui.nest
        (
            [ ( "Cell", Gui.int { min = 60, max = 200, step = 1 } 100 )
            , ( "Shape", Gui.float { min = 0.1, max = 5, step = 0.1 } 1 )
            ] ++
                case fullscreenStatus of
                    Fullscreen ->
                        [ ( "Fullscreen", Gui.toggle True ) ]
                    CustomSize ->
                        [ ( "Fullscreen", Gui.toggle True )
                        -- FIXME: should take values from screen size
                        , ( "Columns", Gui.int { min = 1, max = 100, step = 1 } 13 )
                        , ( "Rows", Gui.int { min = 1, max = 100, step = 1 } 8 )
                        ]
        )
        |> Gui.shape (cols 3)


randomness : GradientStatus -> Logic.TileCount -> Tree ()
randomness gradientStatus tileCount =
     Gui.nest
        [ ( "Diversity", Gui.int { min = 1, max = tileCount, step = 1 } tileCount )
        , ( "Tones", Gui.int { min = 3, max = 17, step = 2 } 3 )
        , ( "Recolor", Gui.button |> Gui.face (icon "update"))
        , ( "Gradient", Gui.toggle False )
        , ( "Gradient size",
                case gradientStatus of
                    Gradient ->
                        Gui.float { min = 4, max = 200, step = 1 } 100
                    Discrete ->
                        Gui.none
          )
        , ( "Smooth color",
                case gradientStatus of
                    Gradient ->
                        Gui.toggle False
                    Discrete ->
                        Gui.none
          )
        ]
        |> Gui.shape (cols 3)


tile : List ( Tileset, TilesetStatus ) -> Tileset -> Tree ()
tile tilesets defaultTileset =
    Gui.nest
        [ ( "Tileset", tileset tilesets defaultTileset )
        , ( "Stroke weight", Gui.int { min = 0, max = 10, step = 1 } 0 )
        , ( "Fill α", Gui.int { min = 0, max = 255, step = 1 } 178 )
        , ( "Stroke α", Gui.int { min = 0, max = 255, step = 1 } 255 )
        ]
        |> Gui.shape (cols 2)


tileset : List ( Tileset, TilesetStatus ) -> Tileset -> Tree ()
tileset tilesets defaultTileset =
    Gui.choice
        (tilesets
            |> Gui.buttons
            --|> List.map (Gui.with (Gui.face << statusIcon << Tuple.second))
            |> Gui.addLabels Tuple.first
            |> Gui.mapSet Tuple.first
        )
        defaultTileset
    |> Gui.cells CellShape.twiceByHalf
    |> Gui.shape (rows 5)
    -- |> Gui.shape (by 1 5)


bgColor : Product.Palette -> Tree ()
bgColor palette =
    Gui.palette
        [ ( "front", palette |> Product.getPaletteColor Product.ColorIII )
        , ( "middle", palette |> Product.getPaletteColor Product.ColorII )
        , ( "rear", palette |>  Product.getPaletteColor Product.ColorI )
        ]
        Color.white
    |> Gui.cells CellShape.single
    -- |> Gui.cells CellShape.twiceByHalf


baseColor : Tree ()
baseColor =
    Gui.palette
        [ ( "default", Color.gray )
        , ( "black", Color.black )
        , ( "white", Color.white )
        ]
        Color.white
    |> Gui.cells CellShape.single
    -- |> Gui.cells CellShape.twiceByHalf


title : Float -> (Int, Int) -> (Int, Int) -> Tree ()
title textCoef (titleX, titleY) (screenX, screenY) =
    Gui.nest
        [ ( "Show", Gui.toggle True)
        , ( "X",
                Gui.int
                    { min = 0, max = screenX, step = 1 }
                     <| min screenX titleX
          )
        , ( "Y",
                Gui.int
                    { min = 0, max = screenY, step = 1 }
                    <| min screenY titleY
          )
        ,( "Scale",
                 Gui.float
                     { min = 0.1, max = 5, step = 0.1 }
                     <| textCoef
         )

        ]
        |> Gui.shape (cols 3)


logo : (Int, Int) -> (Int, Int) -> Tree ()
logo ( amountX, amountY ) ( logoX, logoY ) =
    Gui.nest
        [ ( "Show", Gui.toggle True)
        , ( "X",
                Gui.int
                    { min = 0, max = amountX - 1, step = 1 }
                     <| min (amountX - 1) logoX
          )
        , ( "Y",
                Gui.int
                    { min = 0, max = amountY - 1, step = 1 }
                    <| min (amountY - 1) logoY )
        --, ( "Font size", Gui.int { min = 0, max = 72, step = 1 } 16 )
        --, ( "Opacity", Gui.int { min = 0, max = 255, step = 1 } 255 )
        --, ( "Position",
        --        Gui.xy
        --            ( { min = 0, max = 20, step = 1 }
        --            , { min = 0, max = 20, step = 1 }
        --            )
        --            (14, 18) )
        --, ( "Color",  Gui.color <| Color.rgb255 255 194 0)
        ]
        |> Gui.shape (cols 3)


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


clickAction : Tree ()
clickAction =
    Gui.labels
        actionTypeToString
        actionTypesOrder
        defaultActionType


animation : Tree ()
animation =
    Gui.nest
        [ ( "Animate", Gui.toggle False )
        , ( "Duration", Gui.float { min = 0, max = 4, step = 0.1 } 3 )
        , ( "Delay", Gui.float { min = 0, max = 4, step = 0.1 } 3 )
        ]
        |> Gui.shape (cols 2)
