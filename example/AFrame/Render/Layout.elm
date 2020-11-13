module AFrame.Render.Layout exposing (..)

import Html exposing (Html)

import Color

import AFrame exposing (scene)
import AFrame.Primitives exposing (box)
import AFrame.Primitives.Attributes as A exposing (..)

import Gui exposing (Gui)
import Gui.Msg exposing (Msg)
import Gui.Style.Theme exposing (Theme)
import Gui.Layout exposing (Layout)
import Gui.Layout as Layout exposing (Cell(..), pack, unfold)


widthInCells = 10


heightInCells = 10


scale = 20


view : Theme -> Gui msg -> Html Msg
view theme gui =
    let

        layout = Layout.pack gui.flow ( 10, 10 ) gui.tree
        viewControl ( path, bounds ) =
            [ box
                [ A.color Color.black
                , A.position bounds.x bounds.y -1
                , A.depth 1
                , A.width (bounds.width * 0.9)
                , A.height (bounds.height * 0.9)
                ]
                []
            ]
        viewCell cell =
            case cell of
                One ( path, bounds ) ->
                    viewControl ( path, bounds )
                Many ( parentPath, parentBounds ) innerCells ->
                    viewControl ( parentPath, parentBounds ) ++
                        (innerCells |> List.map viewControl |> List.concat)

    in
        scene
            []
            <| List.concat
            <| List.map viewCell
            <| Layout.toList layout


