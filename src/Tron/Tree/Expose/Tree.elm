module Tron.Tree.Expose.Tree exposing (..)

-- TODO: make controls expose themselves, so get rid of these imports below

import Array as Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Color.Convert as Color
import Maybe.Extra as Maybe
import Task


import Tron.Control as Control exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (Form(..))
import Tron.Control.Impl.Text as Text exposing (TextState(..))
import Tron.Control.Impl.Toggle exposing (ToggleState(..))
import Tron.Control.Impl.Button as Button
import Tron.Control.Impl.XY as XY
import Tron.Path as Path exposing (Path)
-- import Tron.Detach as Detach
import Tron.Tree.Internals as Tree exposing (..)
import Tron.Tree.Controls as Tree
import Tron.Tree.Paths as Tree
import Tron.Tree.Expose.Data as Exp
import Tron.Tree.Expose.Json as Exp
import Tron.Tree.Expose.Convert as Exp
import Tron.Control.Value as Value exposing (Value(..))
-- import Tron.Expose.Convert as Exp



-- FIXME: is it needed at all?
runTree : Value -> Tree x -> Cmd x
runTree value property =
    case ( property, value ) of
        ( Nil _, _ ) ->
            Cmd.none
            -- Task.succeed msg |> Task.perform identity

        ( Number control, FromSlider f ) ->
            control |> Control.update ( Tuple.mapSecond <| always f ) |> Control.run

        ( Coordinate control, FromXY xy ) ->
            control |> Control.update ( Tuple.mapSecond <| always xy ) |> Control.run

        ( Text control, FromInput s ) ->
            control |> Control.update ( Tuple.mapSecond <| always s ) |> Control.run

        ( Color control, FromColor c ) ->
            control |> Control.update ( Tuple.mapSecond <| always c )|> Control.run

        ( Toggle control, FromToggle t ) ->
            control |> setValue t |> Control.run

        ( Action control, FromButton ) ->
            control |> setValue () |> Control.run

        ( Choice _ _ control, FromChoice i ) ->
            control
                |> Control.update
                    (\curValue ->
                        { curValue
                            | selected = i
                        }
                    )
                |> Control.run

        ( Group _ _ _, _ ) ->
            Cmd.none

        ( _, _ ) ->
            Cmd.none


run : Exp.Update -> Tree x -> Cmd x -- FIXME: looks close to `apply`
run { path, value } prop =
    case path of
        [] ->
            runTree value prop

        ( id, _ ) :: next ->
            case prop of
                Group _ _ control ->
                    case control |> Nest.get id of
                        Just ( _, innerProp ) ->
                            innerProp
                                |> run
                                    { path = next
                                    , value = value
                                    }

                        Nothing ->
                            Cmd.none

                _ ->
                    Cmd.none


applyTree : Value -> Tree a -> Tree a
applyTree value prop =
    case ( prop, value ) of
        ( Nil _, _ ) ->
            prop

        ( Number control, FromSlider f ) ->
            control |> Control.update ( Tuple.mapSecond <| always f ) |> Number

        ( Coordinate control, FromXY xy ) ->
            control |> Control.update ( Tuple.mapSecond <| always xy ) |> Coordinate

        ( Text control, FromInput s ) ->
            control |> Control.update ( Tuple.mapSecond <| always s ) |> Text

        ( Color control, FromColor c ) ->
            control |> Control.update ( Tuple.mapSecond <| always c ) |> Color

        ( Toggle control, FromToggle t ) ->
            control |> Control.setValue t |> Toggle

        ( Action control, FromButton ) ->
            control |> Control.setValue () |> Action

        ( Choice focus shape control, FromChoice i ) ->
            Choice focus shape <| Nest.select i <| control

        ( Group _ _ _, _ ) ->
            prop

        ( _, _ ) ->
            prop


apply : Exp.Update -> Tree a -> Tree a
apply { path, value } prop =
    case path of
        [] ->
            applyTree value prop

        ( _, label ) :: next -> -- id :: next
            case prop of
                Group focus shape control ->
                    control
                        {- |> Nest.withItemBy id
                            (Tuple.mapSecond <| apply { path = next, labelPath = labelPath, value = value }) -}
                        |> Nest.withItemAt label
                            (apply
                                { path = next
                                , value = value
                                }
                            )
                        |> Group focus shape

                Choice focus shape control ->
                    control
                        {- |> Nest.withItem id
                            (Tuple.mapSecond <| apply { path = next, value = value }) -}
                        |> Nest.withItemAt label
                            (apply
                                { path = next
                                , value = value
                                }
                            )
                        |> Choice focus shape

                _ ->
                    prop


loadValues : Dict Path Value -> Tree a -> Tree a
loadValues dict prop =
    dict
        |> Dict.toList
        |> List.foldl
            (\ ( path, value ) root ->
                apply { path = Path.toList path, value = value } root
            )
            prop


loadStringValues : Dict (List Path.Label) String -> Tree a -> Tree a
loadStringValues dict =
    Tree.foldP
        (\path innerProp ->
            dict
                |> Dict.get (Path.toLabelPath path)
                |> Maybe.andThen (\strValue -> applyStringValue strValue innerProp)
                |> Maybe.withDefault innerProp
        )


loadJsonValues : Dict (List Path.Index) Exp.Value -> Tree a -> Tree a
loadJsonValues dict prop =
    dict
        |> Dict.toList
        |> List.foldl
            (\ ( path, outUpdate ) root ->
                apply (outUpdate |> Exp.loadValue |> Exp.fromPort) root
            )
            prop
    {- Tron.Tree.fold
        (\path _ root ->
            Dict.get (Path.toList path) dict
                |> Maybe.map (\outUpdate -> apply (outUpdate |> swap |> fromPort) root)
                |> Maybe.withDefault root
        )
        prop
        prop -}


applyStringValue : String -> Tree a -> Maybe (Tree a)
applyStringValue str prop =
    let
        helper typeStr maybeFn =
            str
                |> Exp.fromString typeStr
                |> Result.toMaybe
                |> Maybe.andThen maybeFn
    in
    case prop of
        Nil a ->
            helper
                "none"
                (\v ->
                    case v of
                        None ->
                            Just <| Nil a

                        _ ->
                            Nothing
                )

        Number control ->
            helper
                "slider"
                (\v ->
                    case v of
                        FromSlider n ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always n )
                                |> Number
                                |> Just

                        _ ->
                            Nothing
                )

        Coordinate control ->
            helper
                "xy"
                (\v ->
                    case v of
                        FromXY xy ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always xy )
                                |> Coordinate
                                |> Just

                        _ ->
                            Nothing
                )

        Text control ->
            helper
                "text"
                (\v ->
                    case v of
                        FromInput n ->
                            control
                                |> Text.updateText n
                                |> Text
                                |> Just

                        _ ->
                            Nothing
                )

        Color control ->
            helper
                "color"
                (\v ->
                    case v of
                        FromColor color ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always color )
                                |> Color
                                |> Just

                        _ ->
                            Nothing
                )

        Toggle control ->
            helper
                "toggle"
                (\v ->
                    case v of
                        FromToggle n ->
                            control
                                |> Control.setValue n
                                |> Toggle
                                |> Just

                        _ ->
                            Nothing
                )

        {- Switch control ->
            helper
                "switch"
                (\v ->
                    case v of
                        FromSwitch n ->
                            control
                                |> Control.update ( Tuple.mapSecond <| always n )
                                |> Switch
                                |> Just

                        _ ->
                            Nothing
                ) -}

        Action control ->
            helper
                "button"
                (\v ->
                    case v of
                        FromButton ->
                            Just <| Action control

                        _ ->
                            Nothing
                )

        Choice focus shape control ->
            helper
                "choice"
                (\v ->
                    case v of
                        FromChoice n ->
                            control
                                |> Nest.select n
                                |> Choice focus shape
                                |> Just

                        _ ->
                            Nothing
                )

        Group _ _ _ ->
            Nothing

        Live innerProp ->
            innerProp
                |> applyStringValue str
                |> Maybe.map Live








{- fromPort :
    RawUpdate
    -> Update -- FIXME: -> Result
fromPort portUpdate =
    fromPort1
        { path = portUpdate.path
        , type_ = portUpdate.type_
        , value = portUpdate.value
        } -}




execute : Tree (Maybe msg) -> Cmd msg
execute =
    Tree.get
    >> Maybe.toCommand


{-runExposed : Tree Exp.Update -> Cmd Exp.Out
runExposed prop =
    case Tree.get prop of
        Just rawUpdate ->
            Task.succeed rawUpdate
                |> Task.perform identity
        Nothing -> Cmd.none -}


reflect : Tree a -> Tree Value
reflect prop =
    case prop of
        Nil _ -> Nil None
        Number control ->
            control
                |> Control.move
                |> Control.map (Tuple.second >> FromSlider)
                |> Number
        Coordinate control ->
            control
                |> Control.move
                |> Control.map (Tuple.second >> FromXY)
                |> Coordinate
        Text control ->
            control
                |> Control.move
                |> Control.map (Tuple.second >> FromInput)
                |> Text
        Color control ->
            control
                |> Control.move
                |> Control.map (Tuple.second >> FromColor)
                |> Color
        Toggle control ->
            control
                |> Control.move
                |> Control.map FromToggle
                |> Toggle
        Action control ->
            control
                |> Control.set FromButton
                |> Action
        Choice focus shape control ->
            control
                |> Control.move
                |> Control.map (.selected >> FromSwitch)
                |> Nest.mapItems (Tuple.mapSecond reflect)
                |> Choice focus shape
        Group focus shape control ->
            control
                |> Control.set FromGroup
                |> Nest.mapItems (Tuple.mapSecond reflect)
                |> Group focus shape
        Live innerProp ->
            reflect innerProp
                |> Live


reflectWithPath : Tree a -> Tree ( Path, Value )
reflectWithPath =
    reflect
        >> Tree.pathifyWithValue
    {- Tree.map2
        Tuple.pair
        (pathify prop)
        (reflect prop)
    -}


-- reflect >> pathifyWithValue