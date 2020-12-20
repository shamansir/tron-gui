module AFrame.Render.Layout exposing (..)

import Html exposing (Html)
import Html.Events exposing (..)

import Color

import AFrame exposing (scene, entity)
import AFrame.Primitives.Camera as Aframe exposing (camera)
import AFrame.Primitives.Cursor as Aframe exposing (cursor)
import AFrame.Primitives as Aframe_ exposing (box)
import AFrame.Primitives.Attributes as Aframe exposing (..)
import AFrame.Variants.Primitive as A exposing (..)
import AFrame.Components as AC exposing (..)
import AFrame.Components.Geometry as AG exposing (..)
import AFrame.Components.Geometry.Box as Box exposing (..)
import AFrame.Components.Material as AM exposing (..)
import AFrame.Components.Material.Flat as AM exposing (..)


import Gui exposing (Gui)
import Gui.Msg exposing (Msg(..))
import Gui.Style.Theme exposing (Theme)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (Cell(..), pack, unfold)
import Gui.Property as Gui exposing (find)
import Gui.Path exposing (Path)
import Bounds exposing (Bounds)


widthInCells = 10


heightInCells = 10


scale = 20


z = -10


yOffset = 2


xOffset = -5


view : Theme -> Gui msg -> Html Msg
view theme gui =
    let

        layout = Layout.pack gui.dock ( 20, 20 ) gui.tree
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
            -- , Aframe_.box
            --     [ Aframe.color Color.white
            --     , Aframe.opacity 0.8
            --     , Aframe.position (xOffset + bounds.x) (yOffset + bounds.y * -1) z
            --     , Aframe.depth 0.1
            --     , Aframe.width (bounds.width * 0.9)
            --     , Aframe.height (bounds.height * 0.9)
            --     , onClick <| Click path
            --     ]
            --     []
            , viewProperty bounds path prop
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
            ] ++ -}
            (innerCells
                |> List.map (\(iPath, iBounds) -> viewControl iBounds iPath <| findProp iPath)
                |> List.concat)
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
            <| Aframe.camera [] [ Aframe.cursor [] [] ] :: renderedCells



viewProperty : Bounds -> Path -> Gui.Property msg -> Html Msg
viewProperty bounds path prop =
    Aframe_.box [] []
