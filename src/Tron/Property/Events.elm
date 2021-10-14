module Tron.Property.Events exposing (..)


import Tron.Path as Path exposing (Path)
import Tron.Property as Property exposing (Property(..))
import Tron.Property.Paths exposing (findWithParent)

import Tron.Control.Impl.Text as Text exposing (..)
import Tron.Control.Impl.Toggle as Toggle exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (..)
import Tron.Control.Action as A


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
-- FIXME: should not call controls itself, only return the update
execute : Property a -> Maybe (Property a)
execute item =
    case Property.update A.Execute item of
        ( _, A.Stay ) ->
            Nothing
        ( newCell, _ ) ->
            Just newCell


executeAt : Path -> Property a -> List ( Path, Property a )
executeAt path root =
    case root
        |> findWithParent path of
        Just ( parent, item ) ->
            case ( parent, item ) of
                ( Choice _ _ _, Action _ ) ->

                    case Path.pop path of
                        Just ( toParent, ( selectedIndex, selectedLabel ) ) ->
                            let
                                ( newParent, _ ) = -- FIXME: find not by index but by label?
                                    Property.update (A.Select selectedIndex) parent
                            in
                                case Property.update A.Execute item of
                                    ( _, A.Stay ) ->
                                        [ (toParent, newParent )
                                        ]
                                    ( newCell, _ ) ->
                                        [ ( toParent, newParent )
                                        , ( path, newCell )
                                        ]

                        Nothing ->
                            []

                ( _, _ ) ->

                    case Property.update A.Execute item of
                        ( _, A.Stay ) -> []
                        ( newCell, _ ) -> [ ( path, newCell ) ]

        Nothing -> []
