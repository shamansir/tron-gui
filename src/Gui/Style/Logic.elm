module Gui.Style.Logic exposing (..)


import Dict exposing (Dict)

import Color exposing (Color)
import BinPack exposing (Bounds)

import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CellShape exposing (toString)
import Gui.Style.Coloring exposing (Tone(..))
import Gui.Style.Coloring as Tone exposing (toString, next)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Cell as Cell


import Gui.Mouse exposing (Position)
import Gui.Property as Property exposing (Property(..))
import Gui.Path as Path exposing (Path, toString)
import Gui.Focus exposing (Focused(..))




-- white = Color.white


-- label = Color.rgb255 144 144 144 -- "#909090"



-- canvasBackground = Color.lightGray


-- transparent = Color.rgba 0.0 0.0 0.0 0.0



assignTones : Property msg -> Dict String Tone -- List ( Path, Tone )
assignTones =
    let
        assignTone ( path, prop ) ( knownTones, nextTone ) =
            if Path.howDeep path == 1 then
                case prop of
                    Group _ _ _ ->
                        ( knownTones |> Dict.insert (Path.toString path) nextTone
                        , Tone.next nextTone
                        )
                    Choice _ _ _ ->
                        ( knownTones |> Dict.insert (Path.toString path) nextTone
                        , Tone.next nextTone
                        )
                    _ ->
                        ( knownTones |> Dict.insert (Path.toString path) None
                        , nextTone
                        )
            else -- path is deeper than 0
                case Path.head path of
                    Just sourcePath ->
                        let
                            parentTone =
                                knownTones
                                    |> Dict.get (sourcePath |> Path.toString)
                                    -- |> Dict.get (Path.retract path |> Path.toString)
                                    |> Maybe.withDefault nextTone

                        in
                            ( knownTones |> Dict.insert (Path.toString path) parentTone
                            , nextTone
                            )
                    Nothing ->
                        ( knownTones |> Dict.insert (Path.toString path) None
                        , nextTone
                        )

    in
    Property.unfold
        >> List.sortBy (Tuple.first >> Path.howDeep)
        >> List.foldl assignTone ( Dict.empty, Green )
        >> Tuple.first
        -->> List.map (Tuple.mapFirst Gui.Path.toString)


-- TODO: make bounds to be bounded to pariticular units
toGridCoords : Bounds -> Position -> Position
toGridCoords bounds pos =
    { x = (pos.x - bounds.x) / Cell.width
    , y = (pos.y - bounds.y) / Cell.height
    }


toneToModifier : Tone -> String
toneToModifier=
    Tone.toString


shapeToModifier : CellShape -> String
shapeToModifier =
    CellShape.toString
