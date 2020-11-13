module Gui.Style.Logic exposing (..)


import Dict exposing (Dict)

import Color exposing (Color)
import Gui.Style.Shape exposing (Shape(..))
import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CellShape exposing (toString)
import Gui.Style.Tone exposing (Tone(..))
import Gui.Style.Tone as Tone exposing (toString, next)
import Gui.Style.Theme exposing (Theme)


import Gui.Mouse exposing (Position)
import Bounds exposing (Bounds)
import Gui.Property as Property exposing (Property(..))
import Gui.Path as Path exposing (Path, toString)
import Gui.Focus exposing (Focused(..))




-- white = Color.white


-- label = Color.rgb255 144 144 144 -- "#909090"



-- canvasBackground = Color.lightGray


-- transparent = Color.rgba 0.0 0.0 0.0 0.0


cellWidth : Float
cellWidth = 90


cellHeight : Float
cellHeight = 90


gap = 10


borderRadius = 10


assignTones : Property msg -> Dict String Tone -- List ( Path, Tone )
assignTones =
    let
        assignTone ( path, prop ) ( knownTones, nextTone ) =
            if Path.howDeep path == 1 then
                case prop of
                    Group _ ->
                        ( knownTones |> Dict.insert (Path.toString path) nextTone
                        , Tone.next nextTone
                        )
                    Choice _ ->
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
    { x = (pos.x - bounds.x) / cellWidth
    , y = (pos.y - bounds.y) / cellHeight
    }


toneToModifier : Tone -> String
toneToModifier=
    Tone.toString


shapeToModifier : CellShape -> String
shapeToModifier =
    CellShape.toString
