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
        , ( "Tile", Tron.none )
        , ( "Randomness", Tron.none )
        , ( "Title", Tron.none )
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
