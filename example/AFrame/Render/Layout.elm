module AFrame.Render.Layout exposing (..)

import Html exposing (Html)
import Html.Events exposing (..)

import Color

import AFrame exposing (scene)
import AFrame.Primitives.Camera exposing (camera)
import AFrame.Primitives.Cursor exposing (cursor)
import AFrame.Primitives exposing (box)
import AFrame.Primitives.Attributes as A exposing (..)

import Gui exposing (Gui)
import Gui.Msg exposing (Msg(..))
import Gui.Style.Theme exposing (Theme)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (Cell(..), pack, unfold)


widthInCells = 10


heightInCells = 10


scale = 20


z = -10


yOffset = 2


xOffset = -5


view : Theme -> Gui msg -> Html Msg
view theme gui =
    let

        layout = Layout.pack gui.flow ( 20, 20 ) gui.tree
        viewControl ( path, bounds ) =
            [ box
                [ A.color Color.black
                , A.position (xOffset + bounds.x) (yOffset + bounds.y * -1) z
                , A.depth 0.1
                , A.width (bounds.width * 0.9)
                , A.height (bounds.height * 0.9)
                , onClick <| Click path
                ]
                []
            ]
        viewPlate ( path, bounds ) innerCells =
            {- [ box
                [ A.color Color.red
                , A.position (xOffset + bounds.x) (yOffset + bounds.y * -1) (z - 0.01)
                , A.depth 0.1
                , A.width bounds.width
                , A.height bounds.height
                ]
                []
            ] ++ -} (innerCells |> List.map viewControl |> List.concat)
        viewCell cell =
            case cell of
                One ( path, bounds ) ->
                    viewControl ( path, bounds )
                Many ( parentPath, parentBounds ) innerCells ->
                    viewPlate ( parentPath, parentBounds ) innerCells

        renderedCells =
            List.concat
                <| List.map viewCell
                <| Layout.toList layout
    in
        scene
            []
            <| camera [] [ cursor [] [] ] :: renderedCells
