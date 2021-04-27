module Example.Tiler.Gui exposing (gui)

import Color
import Tron exposing (Tron)
import Tron.Builder.Unit as Tron
import Tron.Style.PanelShape exposing (..)
import Tron.Style.CellShape as CellShape exposing (..)
import Tron.Style.Theme as Theme



gui : Tron ()
gui =
    Tron.root
        [ ( "Color Scheme", Tron.none )
        , ( "Sizes", sizes |> Tron.setIcon (icon "size") )
        , ( "Tile", tile |> Tron.setIcon (icon "tile") )
        , ( "Randomness", randomness |> Tron.setIcon (icon "settings") )
        , ( "Title", title |> Tron.setIcon(icon "text") )
        , ( "Action on click", clickAction |> Tron.setIcon (icon "cursor"))
        , ( "Shuffle color", Tron.buttonWith <| icon "shuffle" )
        , ( "Shuffle tiles", Tron.buttonWith <| icon "shuffle")
        , ( "Change tiles", Tron.buttonWith <| icon "update" )
        , ( "Export", Tron.buttonWith <| icon "export" )
        , ( "Save Scene", Tron.buttonWith <| icon "save" )
        ]


icon : String -> Tron.Icon
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


sizes : Tron ()
sizes =
     Tron.nest
        (cols 3)
        CellShape.single
        [ ( "Cell", Tron.int { min = 0, max = 200, step = 1 } 0 )
        , ( "Shape", Tron.float { min = 0.01, max = 5, step = 0.1 } 0 )
        , ( "Board size", Tron.toggle False )
        ]


randomness : Tron ()
randomness =
     Tron.nest
        (cols 2)
        CellShape.single
        [ ( "Diversity", Tron.int { min = 0, max = 15, step = 1 } 0 )
        ,  ( "Scale", Tron.float { min = 0.1, max = 2, step = 0.1 } 0.1 )
        ,  ( "Ind. fill", Tron.toggle False )
        ,  ( "Colors", Tron.int { min = 1, max = 13, step = 1 } 1 )
        ]


tile : Tron ()
tile =
    Tron.nest
        (cols 2)
        CellShape.single
        [ ("Tileset", tileset)
        , ( "Fill", Tron.int { min = 0, max = 255, step = 1 } 178 )
        , ( "Opacity", Tron.int { min = 0, max = 255, step = 1 } 255 )
        , ( "Stroke", Tron.int { min = 0, max = 10, step = 1 } 1 )
        ]


tileset : Tron ()
tileset =
        Tron.strings
        [ "Geometric", "Fancy", "Lines"] "Foo"


title : Tron()
title =
    Tron.nest
        (cols 3)
        CellShape.single
        [ ("Show", Tron.toggle False)
        , ( "Font size", Tron.int { min = 0, max = 72, step = 1 } 16 )
        , ( "Opacity", Tron.int { min = 0, max = 255, step = 1 } 255 )
        , ("Position", Tron.xy ( { min = -20, max = 20, step = 5 }, { min = -20, max = 20, step = 5 } ) (0, 0))
        , ( "Color",  Tron.color <| Color.rgb255 255 194 0)
        ]


type ActionType
    = ChangeColor
    | ModifyTile
    | ChangeOpacity


actionTypeToString : ActionType -> String
actionTypeToString actionType_ =
    case actionType_ of
        ChangeColor -> "Change Color"
        ModifyTile -> "Modify Tile"
        ChangeOpacity -> "Change Opacity"



clickAction : Tron ()
clickAction =
    Tron.labels
        actionTypeToString
        [ ChangeColor, ModifyTile, ChangeOpacity ]
        ChangeColor
