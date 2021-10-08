module Tron.Property exposing
    ( Property(..), FocusAt(..), Shape, NestShape
    , get, set, setAt, setAll, move, getValue
    , map, map2, map3, map4, map5, mapWithPath, mapWithValue
    , andThen, with
    , toUnit, proxify, lift
    , zip
    , fold, foldP, foldFix, foldZip, foldZipP, unfold
    , updateAt, updateMany
    , perform, apply
    , replaceAt, insideOut
    )


import Task
import Array exposing (Array)
import Dict exposing (Dict)
import Color exposing (Color)
import Size exposing (..)

import Array.Extra.Zipper as Z exposing (zip, Zipper(..))

import Tron.Path exposing (Path)
import Tron.Path as Path

import Tron.Control as Control exposing (..)
import Tron.Control.Button as Button exposing (..)
import Tron.Control.Number as Number exposing (..)
import Tron.Control.XY as XY exposing (..)
import Tron.Control.Text as Text exposing (..)
import Tron.Control.Color as Color exposing (..)
import Tron.Control.Toggle as Toggle exposing (..)
import Tron.Control.Nest as Nest exposing (..)
import Tron.Control.Value exposing (Value(..))

import Tron.Style.CellShape as CS exposing (CellShape)
import Tron.Style.PanelShape as PS exposing (PanelShape)


type FocusAt = FocusAt Int


type alias Shape = ( Float, Float )


type alias NestShape = ( PanelShape, CellShape )


type Property a
    = Nil a
    | Number (Number.Control a)
    | Coordinate (XY.Control a)
    | Text (Text.Control a)
    | Color (Color.Control a)
    | Toggle (Toggle.Control a)
    | Action (Button.Control a)
    | Choice (Maybe FocusAt) NestShape (Nest.ChoiceControl ( Path.Label, Property a ) a)
    | Group (Maybe FocusAt) NestShape (Nest.GroupControl ( Path.Label, Property a ) a)
    | Live (Property a)


map : (a -> b) -> Property a -> Property b
map f prop =
    case prop of
        Nil a -> Nil <| f a
        Number control -> Number <| Control.map f control
        Coordinate control -> Coordinate <| Control.map f control
        Text control -> Text <| Control.map f control
        Color control -> Color <| Control.map f control
        Toggle control -> Toggle <| Control.map f control
        Action control -> Action <| Control.map f control
        Choice focus shape control ->
            Choice
                focus
                shape
                <| (control
                    |> Nest.mapItems (Tuple.mapSecond <| map f)
                    |> Control.map f)
        Group focus shape control ->
            Group
                focus
                shape
                <| (control
                    |> Nest.mapItems (Tuple.mapSecond <| map f)
                    |> Control.map f)
        Live innerProp ->
            Live <| map f innerProp


mapWithPath
    :  (Path -> a -> b)
    -> Property a
    -> Property b
mapWithPath f =
    -- FIXME: should be just another `fold` actually?
    mapHelper <| \path _ a -> f path a



mapWithValue
    :  (Path -> Value -> a -> b)
    -> Property a
    -> Property b
mapWithValue f =
    -- FIXME: should be just another `fold` actually?
    mapHelper <| \path prop a -> f path (getValue prop) a


mapHelper
    :  (Path -> Property a -> a -> b)
    -> Property a
    -> Property b
mapHelper f root =

    let

        mapItemWithPath
            :  Path
            -> Int
            -> ( Path.Label, Property a )
            -> ( Path.Label, Property b )
        mapItemWithPath parentPath index ( label, innerItem ) =
            ( label
            , helper
                ( parentPath |> Path.advance ( index, label ) )
                innerItem
            )

        helper : Path -> Property a -> Property b
        helper curPath item =
            case item of
                Choice focus shape control ->
                    Choice
                        focus
                        shape
                        (control
                            |> Nest.indexedMapItems (mapItemWithPath curPath)
                            |> Control.map (f curPath item)
                        )
                Group focus shape control ->
                    Group
                        focus
                        shape
                        (control
                            |> Nest.indexedMapItems (mapItemWithPath curPath)
                            |> Control.map (f curPath item)
                        )
                Live innerProp ->
                    Live <| helper curPath innerProp
                _ -> map (f curPath item) item

    in
        helper Path.start root


map2 : (a -> b -> c) -> Property a -> Property b -> Property c
map2 f =
    map << get << map f


map3 : (a -> b -> c -> d) -> Property a -> Property b -> Property c -> Property d
map3 f =
    map2 << get << map f


map4 : (a -> b -> c -> d -> e) -> Property a -> Property b -> Property c -> Property d -> Property e
map4 f =
    map3 << get << map f


map5 : (a -> b -> c -> d -> e -> f) -> Property a -> Property b -> Property c -> Property d -> Property e -> Property f
map5 f =
    map4 << get << map f


andThen : (a -> Property b) -> Property a -> Property b
andThen = foldFix
--andThen f = foldP <| always (get >> f)


with : (a -> Property a -> Property b) -> Property a -> Property b
-- FIXME: should be changed to `andThen` with getting rid of function in Control
with f prop =
    andThen (\v -> f v prop) prop


unfold : Property a -> List (Path, Property a)
unfold =
    fold (\path prop prev -> ( path, prop ) :: prev ) []


fold : (Path -> Property a -> b -> b) -> b -> Property a -> b
fold f from root =
    let

        foldItems : Path -> Array ( Path.Label, Property a ) -> b -> b
        foldItems curPath items val =
            items
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, (label, innerItem)) prev ->
                        helper (curPath |> Path.advance (index, label)) innerItem prev
                    )
                    val

        helper : Path -> Property a -> b -> b
        helper curPath prop val =
            case prop of
                Choice _ _ control ->
                    foldItems curPath (Nest.getItems control)
                        <| f curPath prop val
                Group _ _ control ->
                    foldItems curPath (Nest.getItems control)
                        <| f curPath prop val
                -- Live innerProp ->
                --     helper curPath innerProp val
                _ -> f curPath prop val

    in
        helper Path.start root from


foldP : (Path -> Property a -> Property a) -> Property a -> Property a
foldP f root =
    let

        foldItem : Path -> Int -> ( Path.Label, Property a ) -> ( Path.Label, Property a )
        foldItem parentPath index ( label, item ) =
            ( label, helper (parentPath |> Path.advance (index, label)) item )

        helper : Path -> Property a -> Property a
        helper curPath prop =
            case prop of
                Choice focus shape control ->
                    f curPath
                        <| Choice
                            focus
                            shape
                        <| (control
                                |> Nest.indexedMapItems (foldItem curPath))
                Group focus shape control ->
                    f curPath
                        <| Group
                            focus
                            shape
                        <| (control
                                |> Nest.indexedMapItems (foldItem curPath))
                -- Live innerProp ->
                --     helper curPath innerProp val
                _ -> f curPath prop

    in
        helper Path.start root


foldFix : (a -> x) -> Property a -> x
foldFix f prop =
    case prop of
        Nil a -> f a
        Number control -> control |> Control.fold f
        Coordinate control -> control |> Control.fold f
        Text control -> control |> Control.fold f
        Color control -> control |> Control.fold f
        Toggle control -> control |> Control.fold f
        Action control -> control |> Control.fold f
        Choice _ _ control -> control |> Control.fold f -- FIXME: fold through items as well?
        Group _ _ control -> control |> Control.fold f -- FIXME: fold through items as well?
        Live innerProp -> foldFix f innerProp


foldZipP : (Z.Zipper (Property a) (Property b) -> c -> c) -> Property a -> Property b -> c -> c
foldZipP f propA propB = fold2Helper f <| Both propA propB


foldZip : (Z.Zipper a b -> c -> c) -> Property a -> Property b -> c -> c
foldZip f =
    foldZipP (f << Z.mapAB get get)


fold2Helper : (Z.Zipper (Property a) (Property b) -> c -> c) -> Z.Zipper (Property a) (Property b) -> c -> c
fold2Helper f zipper def =
    let
        foldNestItems itemsA itemsB =
            Z.zip
                (itemsA |> Array.map Tuple.second)
                (itemsB |> Array.map Tuple.second)
                |> Array.foldl (fold2Helper f) def
                |> f zipper

    in case Z.toTuple zipper of

        ( Just (Group _ _ groupControlA), Just (Group _ _ groupControlB) ) ->
            foldNestItems
                (Nest.getItems groupControlA)
                (Nest.getItems groupControlB)

        ( Just (Choice _ _ choiceControlA), Just (Choice _ _ choiceControlB) ) ->
            foldNestItems
                (Nest.getItems choiceControlA)
                (Nest.getItems choiceControlB)

        ( Just (Group _ _ groupControlA), _ ) ->
            foldNestItems
                (Nest.getItems groupControlA)
                Array.empty

        ( Just (Choice _ _ choiceControlA), _ ) ->
            foldNestItems
                (Nest.getItems choiceControlA)
                Array.empty

        ( _, Just (Group _ _ groupControlB) ) ->
            foldNestItems
                Array.empty
                (Nest.getItems groupControlB)

        ( _, Just (Choice _ _ choiceControlB) ) ->
            foldNestItems
                Array.empty
                (Nest.getItems choiceControlB)

        ( _, _ ) -> f zipper def


{- fold3 : (Path -> Property a -> b -> b) -> b -> Property a -> b
fold3 f from root =
    -- FIXME: use this one as `fold`, just omit `LabelPath`
    let

        foldItems : Path -> Array ( Path.Label, Property a ) -> b -> b
        foldItems curPath items val =
            items
                --|> Array.map Tuple.second
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, (label, innerItem)) prev ->
                        helper
                            ( curPath |> Path.advance ( index, label ) )
                            innerItem prev
                    )
                    val

        helper : Path -> Property a -> b -> b
        helper curPath prop val =
            case prop of
                Choice _ _ control ->
                    foldItems curPath (Nest.getItems control)
                        <| f curPath prop val
                Group _ _ control ->
                    foldItems curPath (Nest.getItems control)
                        <| f curPath prop val
                -- Live innerProp ->
                --     helper curPath innerProp val
                _ -> f curPath prop val

    in
        helper Path.start root from -}


updateAt : Path -> (Property a -> Property a) -> Property a -> Property a
updateAt path f =
    foldP
        <| \otherPath item ->
            if Path.equal otherPath path then f item else item


updateMany : List ( Path, Property a ) -> Property a -> Property a
updateMany updates root =
    List.foldl
        (\(path, nextProp) lastRoot ->
            lastRoot |> updateAt path (always nextProp)
        )
        root
        updates


replaceAt : Path -> Property a -> Property a -> Property a
replaceAt path newProperty =
    updateAt path <| always newProperty


perform : Property x -> Cmd x
perform prop =
    case prop of
        Nil msg -> Task.succeed msg |> Task.perform identity
        Number control -> control |> Control.run
        Coordinate control -> control |> Control.run
        Text control -> control |> Control.run
        Color control -> control |> Control.run
        Toggle control -> control |> Control.run
        Action control -> control |> Control.run
        Choice _ _ control -> control |> Control.run
        Group _ _ control -> control |> Control.run
        Live innerProp -> perform innerProp


get : Property a -> a
get prop =
    case prop of
        Nil a -> a
        Number control -> control |> Control.get
        Coordinate control -> control |> Control.get
        Text control -> control |> Control.get
        Color control -> control |> Control.get
        Toggle control -> control |> Control.get
        Action control -> control |> Control.get
        Choice _ _ control -> control |> Control.get
        Group _ _ control -> control |> Control.get
        Live innerProp -> get innerProp

set : a -> Property a -> Property a
set a prop =
    case prop of
        Nil _ -> Nil a
        Number control -> Number <| Control.set a <| control
        Coordinate control -> Coordinate <| Control.set a <| control
        Text control -> Text <| Control.set a <| control
        Color control -> Color <| Control.set a <| control
        Toggle control -> Toggle <| Control.set a <| control
        Action control -> Action <| Control.set a <| control
        Choice focus shape control -> Choice focus shape <| Control.set a <| control
        Group focus shape control -> Group focus shape <| Control.set a <| control
        Live innerProp -> Live <| set a innerProp


setAt : Path -> a -> Property a -> Property a
setAt path =
    updateAt path << set


setAll : a -> Property x -> Property a
setAll = map << always


zip : Property a -> Property b -> Property (Z.Zipper a b)
zip =
    let
       join zipper =
            case zipper of
                Z.Both propA_ propB_ -> propB_ |> map (Z.Both <| get propA_)
                Z.Right propA_ -> propA_ |> map Z.Right
                Z.Left propB_ -> propB_ |> map Z.Left
    in move join


move
     : (Z.Zipper (Property a) (Property b) -> Property c)
    -> Property a
    -> Property b
    -> Property c
move f propA propB = moveHelper f <| Z.Both propA propB


moveHelper
     : (Z.Zipper (Property a) (Property b) -> Property c)
    -> Z.Zipper (Property a) (Property b)
    -> Property c
moveHelper f zipper =
    let
        merge
             : Z.Zipper ( Path.Label, Property a ) ( Path.Label, Property b )
            -> ( Path.Label, Property c )
        merge zipperCursor =
            case zipperCursor of
                Z.Both (labelA, propA_) (labelB, propB_) ->
                    (labelB, moveHelper f <| Both propA_ propB_)
                Z.Left (labelA, propA_) ->
                    (labelA, moveHelper f <| Z.Left propA_)
                Z.Right (labelB, propB_) ->
                    (labelB, moveHelper f <| Z.Right propB_)
        zipItems
             : NestControl (Path.Label, Property a) value a
            -> NestControl (Path.Label, Property b) value b
            -> Array (Path.Label, Property c)
        zipItems controlA controlB =
            Z.zip
                (Nest.getItems controlA)
                (Nest.getItems controlB)
                |> Array.map merge
    in
    case zipper of
        Z.Both (Choice _ _ controlA) (Choice _ _ controlB) ->
            case f zipper of
                Choice focus shape control ->
                    Choice focus shape <| Nest.setItems (zipItems controlA controlB) <| control
                otherProp -> otherProp
        Z.Both (Group _ _ controlA) (Group _ _ controlB) ->
            case f zipper of
                Group focus shape control ->
                    Group focus shape <| Nest.setItems (zipItems controlA controlB) <| control
                otherProp -> otherProp
        _ -> f zipper


insideOut : Property ( a, b ) -> ( a, Property b )
insideOut prop =
    ( get prop |> Tuple.first, prop |> map Tuple.second )


{-| Replace the `()` value everywhere within Tron GUI tree, it is useful for truly a lot of cases when you don't care about what are the associated values.
-}
toUnit : Property a -> Property ()
toUnit = setAll ()


proxify : Property a -> Property Value
proxify = mapWithValue <| \_ val _ -> val


{-| convert usual `Tron a` to `Tron.OfValue a`. Please prefer the one from the `Tron.OfValue` module. -}
lift : Property a -> Property (Value -> Maybe a)
lift =
    map (always << Just)


apply : Property (Value -> Maybe a) -> Property (Maybe a)
apply = mapWithValue (\_ val handler -> handler val)



{-| get proxied value from `Tron` -}
getValue : Property a -> Value
getValue prop =
    case prop of
        Nil _ -> None
        Number control -> control |> Control.getValue |> Tuple.second |> FromSlider
        Coordinate control -> control |> Control.getValue |> Tuple.second |> FromXY
        Text control -> control |> Control.getValue |> Tuple.second |> FromInput
        Color control -> control |> Control.getValue |> Tuple.second |> FromColor
        Toggle control -> control |> Control.getValue |> FromToggle
        Action _ -> FromButton
        Choice _ _ control -> control |> Control.getValue |> .selected |> FromChoice
        Group _ _ _ -> FromGroup
        Live innerProp -> getValue innerProp