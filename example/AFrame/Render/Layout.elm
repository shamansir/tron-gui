module AFrame.Render.Layout exposing (..)

import AFrame exposing (entity, scene)
import AFrame.Components as AC exposing (..)
import AFrame.Components.Geometry as AG exposing (..)
import AFrame.Components.Geometry.Box as Box exposing (..)
import AFrame.Components.Geometry.Cylinder as Cylinder exposing (..)
import AFrame.Components.Material as AM exposing (..)
import AFrame.Components.Material.Flat as AM exposing (..)
import AFrame.Components.Text as AT exposing (..)
import AFrame.Primitives as Aframe_ exposing (box)
import AFrame.Primitives.Attributes as Aframe exposing (..)
import AFrame.Primitives.Camera as Aframe exposing (camera)
import AFrame.Primitives.Cursor as Aframe exposing (cursor)
import AFrame.Variants.Primitive as A exposing (..)

import Size exposing (SizeF(..))
import BinPack exposing (Bounds)
import Color
import Html exposing (Html)
import Html.Events exposing (..)
import Tron exposing (Tron)
import Tron.Control as Gui exposing (Control(..))
import Tron.Control.Button as Button exposing (Face(..), Icon(..))
import Tron.Control.Toggle as Gui exposing (ToggleState(..))
import Tron.Layout as Layout exposing (Cell(..), Layout, pack, fold)
import Tron.Msg exposing (Msg_(..))
import Tron.Path exposing (Path)
import Tron.Property as Gui exposing (find)
import Tron.Style.Theme exposing (Theme)


widthInCells =
    10


heightInCells =
    10


scale =
    20


z =
    -10


yOffset =
    2


xOffset =
    -5


view : Theme -> Model msg -> Html Msg_
view theme gui =
    let
        layout =
            Layout.pack gui.dock (SizeF ( 20, 20 )) gui.tree

        findProp path =
            Gui.find path gui.tree
                |> Maybe.withDefault Gui.Nil

        viewControl bounds path prop =
            [ entity
                [ geometry
                    [ AG.primitive A.box
                    , Box.depth 0.1
                    , Box.width (bounds.width * 0.9)
                    , Box.height (bounds.height * 0.9)
                    ]
                , AC.position (xOffset + bounds.x) (yOffset + bounds.y * -1) z
                , material
                    [ AM.color Color.white
                    , AM.opacity 0.8
                    ]
                , onClick <| Click path
                ]
                []
            , entity
                [ onClick <| Click path ]
                [ viewProperty bounds path prop ]
            ]

        viewPlate bounds path prop innerCells =
            {- [ box
                   [ A.color Color.red
                   , A.position (xOffset + bounds.x) (yOffset + bounds.y * -1) (z - 0.01)
                   , A.depth 0.1
                   , A.width bounds.width
                   , A.height bounds.height
                   ]
                   []
               ] ++
            -}
            innerCells
                |> List.map (\( iPath, iBounds ) -> viewControl iBounds iPath <| findProp iPath)
                |> List.concat

        viewCell cell =
            case cell of
                One ( path, bounds ) ->
                    viewControl bounds path <| findProp path

                Many ( parentPath, parentBounds ) innerCells ->
                    viewPlate parentBounds parentPath (findProp parentPath) innerCells

        renderedCells =
            layout
                |> Layout.toList
                |> List.map viewCell
                |> List.concat
    in
    scene
        []
    -- [ debug ]
    <|
        Aframe.camera [] [ Aframe.cursor [] [] ]
            :: renderedCells


viewProperty : Bounds -> Path -> Gui.Property a -> Html Msg_
viewProperty bounds path prop =
    case prop of
        Gui.Number (Gui.Control cfg val _) ->
            entity
                []
                [ entity
                    [ geometry
                        [ AG.primitive A.cylinder
                        , Cylinder.radius (min bounds.width bounds.height * 0.4)
                        , Cylinder.height 0.05
                        ]
                    , AC.position (xOffset + bounds.x) (yOffset + bounds.y * -1) (z + 0.05)
                    , AC.rotation 90 0 0
                    , material
                        [ AM.color <| Color.rgb 0.7 0.7 0.7
                        , AM.opacity 0.8
                        ]
                    ]
                    []
                , entity
                    [ geometry
                        [ AG.primitive A.box
                        , Box.depth 0.1
                        , Box.width 0.05
                        , Box.height (bounds.height * 0.8)
                        ]

                    {-
                       [ AG.primitive A.cylinder
                       , Cylinder.radius (min bounds.width bounds.height * 0.4)
                       , Cylinder.height 0.05
                       , Cylinder.thetaStart 0
                       , Cylinder.thetaLength <| (val - cfg.min) / (cfg.max - cfg.min) * 360
                       ]
                    -}
                    , AC.position
                        (xOffset + bounds.x)
                        (yOffset + (bounds.y * -1))
                        (z + 0.2)

                    -- , for cylinder: AC.rotation 90 0 0
                    , AC.rotation 0 0 <| (val - cfg.min) / (cfg.max - cfg.min) * 360
                    , material
                        [ AM.color <| Color.rgb 0.2 0.2 0.2
                        , AM.opacity 0.8
                        ]
                    ]
                    []
                ]

        Gui.Coordinate (Gui.Control ( xcfg, ycfg ) ( x, y ) _) ->
            entity
                [ geometry
                    [ AG.primitive A.cylinder
                    , Cylinder.radius 0.1
                    , Cylinder.height 0.05
                    ]
                , let
                    sideX =
                        (xcfg.max - xcfg.min) * (bounds.width * 0.02)

                    sideY =
                        (ycfg.max - ycfg.min) * (bounds.height * 0.02)

                    scX =
                        x / (xcfg.max - xcfg.min) * (bounds.width * 0.02)

                    scY =
                        y / (ycfg.max - ycfg.min) * (bounds.height * 0.02)
                  in
                  AC.position
                    (xOffset + bounds.x - (sideX / 2) + scX)
                    (yOffset + (bounds.y * -1) + (sideY / 2) + scY)
                    (z + 0.2)
                , AC.rotation 90 0 0
                , material
                    [ AM.color <| Color.rgb 0.2 0.2 0.2
                    , AM.opacity 0.8
                    ]
                ]
                []

        Gui.Text (Gui.Control _ ( _, val ) _) ->
            entity
                [ text
                    [ AT.value val
                    , AT.width (bounds.width * 4)
                    , AT.color <| Color.rgb 0.2 0.2 0.2

                    --, AT.lineHeight 0.1
                    ]
                , AC.position
                    (xOffset + bounds.x + 1.9)
                    (yOffset + (bounds.y * -1) + 0.1)
                    (z + 0.4)
                , material
                    [ AM.color <| Color.rgb 0.2 0.2 0.2
                    , AM.opacity 0.8
                    ]
                ]
                []

        Gui.Toggle (Gui.Control _ val _) ->
            entity
                [ geometry
                    [ AG.primitive A.cylinder
                    , Cylinder.radius 0.1
                    , Cylinder.height 0.05
                    ]
                , AC.position
                    (xOffset + bounds.x)
                    (yOffset + (bounds.y * -1))
                    (z + 0.2)
                , AC.rotation 90 0 0
                , material
                    [ AM.color <|
                        case val of
                            TurnedOn ->
                                Color.green

                            TurnedOff ->
                                Color.red
                    , AM.opacity 0.8
                    ]
                ]
                []

        Gui.Action (Gui.Control face _ _) ->
            entity
                [ text
                    [ AT.value <|
                        case face of
                            Button.Default ->
                                "btn"

                            Button.WithIcon (Icon icon) ->
                                icon

                            Button.WithColor color ->
                                Color.toCssString color
                    , AT.width (bounds.width * 4)
                    , AT.color <| Color.rgb 0.2 0.2 0.2

                    --, AT.lineHeight 0.1
                    ]
                , AC.position
                    (xOffset + bounds.x + 1.9)
                    (yOffset + (bounds.y * -1) + 0.1)
                    (z + 0.4)
                , material
                    [ AM.color <| Color.rgb 0.2 0.2 0.2
                    , AM.opacity 0.8
                    ]
                ]
                []

        Gui.Color (Gui.Control _ curVal _) ->
            entity
                [ geometry
                    [ AG.primitive A.cylinder
                    , Cylinder.radius (min bounds.width bounds.height * 0.4)
                    , Cylinder.height 0.2
                    ]
                , AC.position (xOffset + bounds.x) (yOffset + bounds.y * -1) (z + 0.05)
                , AC.rotation 90 0 0
                , material
                    [ let
                        cc =
                            Color.toCssString curVal

                        -- FIXME: converts to `rbga`, Three.js is not accepting it
                      in
                      AM.color <| curVal
                    , AM.opacity 0.8
                    ]
                , onClick <| Click path
                ]
                []

        _ ->
            Aframe_.box [] []
