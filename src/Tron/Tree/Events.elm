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
execute : Tree () -> Tree A.Change
execute =
    Tree.update A.Execute


executeAt : Path -> Tree () -> List ( Path, Tree A.Change )
executeAt path root =
    Debug.log "changes" <| case root
        |> findWithParent path of
        Just ( parent, item ) ->
            case ( parent, item ) of
                ( Choice _ _ _, Action _ ) ->

                    case Path.pop path of
                        Just ( toParent, ( selectedIndex, selectedLabel ) ) ->
                            let
                                newParent = -- FIXME: find not by index but by label?
                                    Tree.update (A.Select selectedIndex) parent
                                newCell =
                                    Tree.update A.Execute item
                            in
                                case newCell |> Tree.get of
                                    A.Stay ->
                                        [ ( toParent, newParent )
                                        ]
                                    _ ->
                                        [ ( toParent, newParent )
                                        , ( path, newCell )
                                        ]

                        Nothing ->
                            []

                ( _, _ ) ->

                    let
                        newCell = Tree.update A.Execute item
                    in
                    case newCell |> Tree.get of
                        A.Stay -> []
                        _ -> [ ( path, newCell ) ]

        Nothing -> []
