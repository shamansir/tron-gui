module Tron.Property.Events exposing (..)


import Tron.Path as Path exposing (Path)
import Tron.Property exposing (Property(..))
import Tron.Property.Paths exposing (findWithParent)

import Tron.Control.Text as Text exposing (..)
import Tron.Control.Toggle as Toggle exposing (..)
import Tron.Control.Nest as Nest exposing (..)


-- for mouse click or enter key handling, does not change the tree
-- only updates the controls itself
-- FIXME: should not call controls itself, only return the update
execute : Property a -> Maybe (Property a)
execute item =
    case item of
        Toggle toggleControl ->
            Just <| Toggle <| Toggle.toggle toggleControl
        Action control ->
            -- we don't update the value since it's `()`, but we do execute it
            Just <| Action control
        Text textControl ->
            Just <| Text <| Text.ensureEditing textControl
        Choice focus shape control ->
            case Nest.getChoiceMode control of
                Nest.Pages ->
                    Just
                        <| Choice focus shape
                        <| Nest.toggle control
                Nest.Knob ->
                    Just
                        <| Choice focus shape
                        <| Nest.toNext control
                Nest.SwitchThrough ->
                    Just
                        <| Choice focus shape
                        <| Nest.toNext control
        Group focus shape control ->
            Just
                <| Group focus shape
                <| Nest.toggle control
        Live innerProp ->
            execute innerProp
                |> Maybe.map Live
        _ -> Nothing


executeAt : Path -> Property a -> List ( Path, Property a )
executeAt path root =
    case root
        |> findWithParent path of
        Just ( parent, item ) ->
            case ( parent, item ) of
                ( Choice focus shape control, Action _ ) ->

                    case Path.pop path of
                        Just ( toParent, ( selectedIndex, selectedLabel ) ) ->
                            let
                                newParent = -- FIXME: find not by index but by label?
                                    select selectedIndex control
                            in
                                case execute item of
                                    Just newCell ->
                                        [ ( toParent, Choice focus shape newParent )
                                        , ( path, newCell )
                                        ]
                                    Nothing ->
                                        [ (toParent, Choice focus shape newParent )
                                        ]
                        Nothing ->
                            []

                ( _, _ ) ->

                    case execute item of
                        Just newCell -> [ ( path, newCell ) ]
                        Nothing -> []

        Nothing -> []
