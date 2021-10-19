module Tron.Tree.Events exposing (..)


import Tron.Path as Path exposing (Path)
import Tron.Tree as Tree exposing (Tree(..))
import Tron.Tree.Paths exposing (findWithParent)

import Tron.Control.Impl.Text as Text exposing (..)
import Tron.Control.Impl.Toggle as Toggle exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (..)
import Tron.Control.Action as A


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
-- FIXME: should not call controls itself, only return the update
execute : Tree a -> Maybe (Tree a)
execute item =
    case Tree.update A.Execute item of
        ( _, A.Stay ) ->
            Nothing
        ( newCell, _ ) ->
            Just newCell


executeAt : Path -> Tree a -> List ( Path, Tree a )
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
                                    Tree.update (A.Select selectedIndex) parent
                            in
                                case Tree.update A.Execute item of
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

                    case Tree.update A.Execute item of
                        ( _, A.Stay ) -> []
                        ( newCell, _ ) -> [ ( path, newCell ) ]

        Nothing -> []
