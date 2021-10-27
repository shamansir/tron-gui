module Tron.Tree.Internals exposing
    ( Tree(..), FocusAt(..), Shape, NestShape
    , empty
    , get, set, setAt, setAll, move, getValue
    , map, mapWithPath, mapWithValue
    , zipMap2, zipMap3, zipMap4, zipMap5
    , zipJoinMap2, zipJoinMap3, zipJoinMap4, zipJoinMap5
    , squeezeMap2, squeezeMap3, squeezeMap4, squeezeMap5
    , andThen, with
    , toUnit, proxify
    , zip
    , fold, foldP, foldFix, foldZip, foldZipP, unfold
    , updateAt, updateMany
    , perform, apply, update
    , replaceAt, insideOut
    )


import Task
import Array exposing (Array)
import Color exposing (Color)
import Size exposing (..)

import Array.Extra.Zipper as Z exposing (zip, Zipper(..))

import Tron.Path exposing (Path)
import Tron.Path as Path

import Tron.Control as Control exposing (..)
import Tron.Control.Impl.Button as Button exposing (..)
import Tron.Control.Impl.Number as Number exposing (..)
import Tron.Control.Impl.XY as XY exposing (..)
import Tron.Control.Impl.Text as Text exposing (..)
import Tron.Control.Impl.Color as Color exposing (..)
import Tron.Control.Impl.Toggle as Toggle exposing (..)
import Tron.Control.Impl.Nest as Nest exposing (..)
import Tron.Control.Value exposing (Value(..))
import Tron.Control.Action as A

import Tron.Style.CellShape as CS exposing (CellShape)
import Tron.Style.PanelShape as PS exposing (PanelShape)


type FocusAt = FocusAt Int


type alias Shape = ( Float, Float )


type alias NestShape = ( PanelShape, CellShape )


type Tree a
    = Nil a
    | Number (Number.Control a)
    | Coordinate (XY.Control a)
    | Text (Text.Control a)
    | Color (Color.Control a)
    | Toggle (Toggle.Control a)
    | Action (Button.Control a)
    | Choice (Maybe FocusAt) NestShape (Nest.ChoiceControl ( Path.Label, Tree a ) a)
    | Group (Maybe FocusAt) NestShape (Nest.GroupControl ( Path.Label, Tree a ) a)
    | Live (Tree a)


empty : Tree ()
empty = Nil ()


map : (a -> b) -> Tree a -> Tree b
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
    -> Tree a
    -> Tree b
mapWithPath f =
    -- FIXME: should be just another `fold` actually?
    mapHelper <| \path _ a -> f path a



mapWithValue
    :  (Path -> Value -> a -> b)
    -> Tree a
    -> Tree b
mapWithValue f =
    -- FIXME: should be just another `fold` actually?
    mapHelper <| \path prop a -> f path (getValue prop) a


mapHelper
    :  (Path -> Tree a -> a -> b)
    -> Tree a
    -> Tree b
mapHelper f root =

    let

        mapItemWithPath
            :  Path
            -> Int
            -> ( Path.Label, Tree a )
            -> ( Path.Label, Tree b )
        mapItemWithPath parentPath index ( label, innerItem ) =
            ( label
            , helper
                ( parentPath |> Path.advance ( index, label ) )
                innerItem
            )

        helper : Path -> Tree a -> Tree b
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


andThen : (a -> Tree b) -> Tree a -> Tree b
andThen = foldFix
--andThen f = foldP <| always (get >> f)


with : (a -> Tree a -> Tree b) -> Tree a -> Tree b
-- FIXME: should be changed to `andThen` with getting rid of function in Control
with f prop =
    andThen (\v -> f v prop) prop


unfold : Tree a -> List (Path, Tree a)
unfold =
    fold (\path prop prev -> ( path, prop ) :: prev ) []


fold : (Path -> Tree a -> b -> b) -> b -> Tree a -> b
fold f from root =
    let

        foldItems : Path -> Array ( Path.Label, Tree a ) -> b -> b
        foldItems curPath items val =
            items
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, (label, innerItem)) prev ->
                        helper (curPath |> Path.advance (index, label)) innerItem prev
                    )
                    val

        helper : Path -> Tree a -> b -> b
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


foldP : (Path -> Tree a -> Tree a) -> Tree a -> Tree a
foldP f root =
    let

        foldItem : Path -> Int -> ( Path.Label, Tree a ) -> ( Path.Label, Tree a )
        foldItem parentPath index ( label, item ) =
            ( label, helper (parentPath |> Path.advance (index, label)) item )

        helper : Path -> Tree a -> Tree a
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


foldFix : (a -> x) -> Tree a -> x
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


foldZipP : (Z.Zipper (Tree a) (Tree b) -> c -> c) -> Tree a -> Tree b -> c -> c
foldZipP f propA propB = fold2Helper f <| Both propA propB


foldZip : (Z.Zipper a b -> c -> c) -> Tree a -> Tree b -> c -> c
foldZip f =
    foldZipP (f << Z.mapAB get get)


fold2Helper : (Z.Zipper (Tree a) (Tree b) -> c -> c) -> Z.Zipper (Tree a) (Tree b) -> c -> c
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


{- fold3 : (Path -> Tree a -> b -> b) -> b -> Tree a -> b
fold3 f from root =
    -- FIXME: use this one as `fold`, just omit `LabelPath`
    let

        foldItems : Path -> Array ( Path.Label, Tree a ) -> b -> b
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

        helper : Path -> Tree a -> b -> b
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


updateAt : Path -> (Tree a -> Tree a) -> Tree a -> Tree a
updateAt path f =
    foldP
        <| \otherPath item ->
            if Path.equal otherPath path then f item else item


updateMany : List ( Path, Tree a ) -> Tree a -> Tree a
updateMany updates root =
    List.foldl
        (\(path, nextProp) lastRoot ->
            lastRoot |> updateAt path (always nextProp)
        )
        root
        updates


replaceAt : Path -> Tree a -> Tree a -> Tree a
replaceAt path newTree =
    updateAt path <| always newTree


perform : Tree x -> Cmd x
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


get : Tree a -> a
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

set : a -> Tree a -> Tree a
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


setAt : Path -> a -> Tree a -> Tree a
setAt path =
    updateAt path << set


setAll : a -> Tree x -> Tree a
setAll = map << always


{- When two properties match at the same place in two given trees, the state is taken from the second (right) one. -}
zip : Tree a -> Tree b -> Tree (Z.Zipper a b)
zip =
    let
       join zipper =
            case zipper of
                Z.Both propA_ propB_ -> propB_ |> map (Z.Both <| get propA_)
                Z.Right propA_ -> propA_ |> map Z.Right
                Z.Left propB_ -> propB_ |> map Z.Left
    in move join



{- `zipMap2` maps two properties using zipper with given `fn`; if tree structure doesn't match while zipping, the corresponding `Maybe`s
become `Nothing`.
When two properties match at the same place in two given trees, the state is taken from the second (right) one. -}
zipMap2 : (Maybe a -> Maybe b -> c) -> Tree a -> Tree b -> Tree c
zipMap2 f propA propB =
    zip propA propB
        |> map (Z.fold f)


{- `zipJoinMap2` maps two properties using zipper with given `fn`; if tree structure doesn't match while zipping, the subject
of the resulting prop becomes `Nothing`.
When two properties match at the same place in two given trees, the state is taken from the second (right) one. -}
zipJoinMap2 : (a -> b -> c) -> Tree a -> Tree b -> Tree (Maybe c)
zipJoinMap2 = zipMap2 << Maybe.map2


{- `squeezeMap2` maps two properties using zipper with given `fn`; if tree structure doesn't match while zipping,
the subjects for the function are taken from the given properties (which can be dangerous, use it on your own risk!).
When two properties match at the same place in two given trees, state is taken from the second (right) one.
You are safe if you use the same tree as both sources though.
For example:

    Tree.squeezeMap2
        Tuple.pair
        (Tree.pathify myTree)
        (Tree.proxify myTree)
-}
squeezeMap2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
squeezeMap2 f propA propB =
    zipMap2
        (\maybeA maybeB ->
            f
                (maybeA |> Maybe.withDefault (get propA))
                (maybeB |> Maybe.withDefault (get propB))
        )
        propA propB


zipMapHelper : (Maybe a -> b) -> Maybe (Maybe a -> b) -> Maybe a -> b
zipMapHelper nf maybeFn maybeLastVal =
    case ( maybeFn, maybeLastVal ) of
        ( Just fn, Just lastVal ) -> fn <| Just lastVal
        ( Just fn, Nothing ) -> fn Nothing
        ( Nothing, Just lastVal ) -> nf <| Just lastVal
        ( Nothing, Nothing ) -> nf Nothing


{- `zipMap3` maps three properties using zipper with given `fn`; if tree structure doesn't match while zipping, the corresponding `Maybe`s
become `Nothing` -}
zipMap3 : (Maybe a -> Maybe b -> Maybe c -> d) -> Tree a -> Tree b -> Tree c -> Tree d
zipMap3 f propA propB propC =
    zip
       (zipMap2 f propA propB)
       propC
       |> map
            (Z.fold <| zipMapHelper <| f Nothing Nothing)


{- `zjmap3` maps three properties using zipper with given `fn`; if tree structure doesn't match while zipping, the subject
of the resulting prop becomes `Nothing` -}
zipJoinMap3 : (a -> b -> c -> d) -> Tree a -> Tree b -> Tree c -> Tree (Maybe d)
zipJoinMap3 = zipMap3 << Maybe.map3


{- `squeezeMap3` maps three properties using zipper with given `fn`; if tree structure doesn't match while zipping,
the subjects for the function are taken from the given properties (which can be dangerous, use it on your own risk!) -}
squeezeMap3 : (a -> b -> c -> d) -> Tree a -> Tree b -> Tree c -> Tree d
squeezeMap3 f propA propB propC =
    zipMap3
        (\maybeA maybeB maybeC ->
            f
                (maybeA |> Maybe.withDefault (get propA))
                (maybeB |> Maybe.withDefault (get propB))
                (maybeC |> Maybe.withDefault (get propC))
        )
        propA propB propC


{- `zipMap4` maps four properties using zipper with given `fn`; if tree structure doesn't match while zipping, the corresponding `Maybe`s
become `Nothing` -}
zipMap4 : (Maybe a -> Maybe b -> Maybe c -> Maybe d -> e) -> Tree a -> Tree b -> Tree c -> Tree d -> Tree e
zipMap4 f propA propB propC propD =
    zip
       (zipMap3 f propA propB propC)
       propD
       |> map
            (Z.fold <| zipMapHelper <| f Nothing Nothing Nothing)


{- `zipJoinMap4` maps four properties using zipper with given `fn`; if tree structure doesn't match while zipping, the subject
of the resulting prop becomes `Nothing` -}
zipJoinMap4 : (a -> b -> c -> d -> e) -> Tree a -> Tree b -> Tree c -> Tree d -> Tree (Maybe e)
zipJoinMap4 = zipMap4 << Maybe.map4


{- `sqeezeMap4` maps four properties using zipper with given `fn`; if tree structure doesn't match while zipping,
the subjects for the function are taken from the given properties (which can be dangerous, use it on your own risk!) -}
squeezeMap4 : (a -> b -> c -> d -> e) -> Tree a -> Tree b -> Tree c -> Tree d -> Tree e
squeezeMap4 f propA propB propC propD =
    zipMap4
        (\maybeA maybeB maybeC maybeD ->
            f
                (maybeA |> Maybe.withDefault (get propA))
                (maybeB |> Maybe.withDefault (get propB))
                (maybeC |> Maybe.withDefault (get propC))
                (maybeD |> Maybe.withDefault (get propD))
        )
        propA propB propC propD


{- `zipMap5` maps five properties using zipper with given `fn`; if tree structure doesn't match while zipping, the corresponding `Maybe`s
become `Nothing` -}
zipMap5 : (Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> f) -> Tree a -> Tree b -> Tree c -> Tree d -> Tree e -> Tree f
zipMap5 f propA propB propC propD propE =
    zip
       (zipMap4 f propA propB propC propD)
       propE
       |> map
            (Z.fold <| zipMapHelper <| f Nothing Nothing Nothing Nothing)


{- `zipJoinMap5` maps five properties using zipper with given `fn`; if tree structure doesn't match while zipping, the subject
of the resulting prop becomes `Nothing` -}
zipJoinMap5 : (a -> b -> c -> d -> e -> f) -> Tree a -> Tree b -> Tree c -> Tree d -> Tree e -> Tree (Maybe f)
zipJoinMap5 = zipMap5 << Maybe.map5


{- `sqeezeMap5` maps four properties using zipper with given `fn`; if tree structure doesn't match while zipping,
the subjects for the function are taken from the given properties (which can be dangerous, use it on your own risk!) -}
squeezeMap5 : (a -> b -> c -> d -> e -> f) -> Tree a -> Tree b -> Tree c -> Tree d -> Tree e -> Tree f
squeezeMap5 f propA propB propC propD propE =
    zipMap5
        (\maybeA maybeB maybeC maybeD maybeE ->
            f
                (maybeA |> Maybe.withDefault (get propA))
                (maybeB |> Maybe.withDefault (get propB))
                (maybeC |> Maybe.withDefault (get propC))
                (maybeD |> Maybe.withDefault (get propD))
                (maybeE |> Maybe.withDefault (get propE))
        )
        propA propB propC propD propE


move
     : (Z.Zipper (Tree a) (Tree b) -> Tree c)
    -> Tree a
    -> Tree b
    -> Tree c
move f propA propB = moveHelper f <| Z.Both propA propB


moveHelper
     : (Z.Zipper (Tree a) (Tree b) -> Tree c)
    -> Z.Zipper (Tree a) (Tree b)
    -> Tree c
moveHelper f zipper =
    let
        merge
             : Z.Zipper ( Path.Label, Tree a ) ( Path.Label, Tree b )
            -> ( Path.Label, Tree c )
        merge zipperCursor =
            case zipperCursor of
                Z.Both (labelA, propA_) (labelB, propB_) ->
                    (labelB, moveHelper f <| Both propA_ propB_)
                Z.Left (labelA, propA_) ->
                    (labelA, moveHelper f <| Z.Left propA_)
                Z.Right (labelB, propB_) ->
                    (labelB, moveHelper f <| Z.Right propB_)
        zipItems
             : NestControl (Path.Label, Tree a) value a
            -> NestControl (Path.Label, Tree b) value b
            -> Array (Path.Label, Tree c)
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


insideOut : Tree ( a, b ) -> ( a, Tree b )
insideOut prop =
    ( get prop |> Tuple.first, prop |> map Tuple.second )



{-| Replace the `()` subject everywhere within Tron GUI tree, it is useful for truly a lot of cases when you don't care about what are the associated values.
-}
toUnit : Tree a -> Tree ()
toUnit = setAll ()


proxify : Tree a -> Tree Value
proxify = mapWithValue <| \_ val _ -> val


apply : Tree (Value -> Maybe a) -> Tree (Maybe a)
apply = mapWithValue (\_ val handler -> handler val)



{-| get proxied value from `Tron` -}
getValue : Tree a -> Value
getValue prop =
    case prop of
        Nil _ -> None
        Number control -> control |> Control.getValue |> Tuple.second |> FromSlider
        Coordinate control -> control |> Control.getValue |> Tuple.second |> FromXY
        Text control -> control |> Control.getValue |> Tuple.second |> FromInput
        Color control -> control |> Control.getValue |> Tuple.second |> FromColor
        Toggle control -> control |> Control.getValue |> Toggle.toggleToBool |> FromToggle
        Action _ -> FromButton
        Choice _ _ control -> control |> Nest.getSelectedAsPair |> Tuple.mapSecond (Maybe.map Tuple.first) |> FromChoice
        Group _ _ _ -> FromGroup
        Live innerProp -> getValue innerProp


update : A.Action -> Tree () -> Tree A.Change
update action prop =
    let
        swap ( control, change ) = control |> Control.set change -- FIXME: make Controls' `update` act the same
    in
    case prop of
        Nil _ -> ( Nil A.Stay )
        Number control -> control |> Number.update action |> swap |> Number
        Coordinate control -> control |> XY.update action |> swap |> Coordinate
        Text control -> control |> Text.update action |> swap |> Text
        Color control -> control |> Color.update action |> swap |> Color
        Toggle control -> control |> Toggle.update action |> swap |> Toggle
        Action control -> control |> Button.update action |> swap |> Action
        Choice focus shape control ->
            control
                |> Nest.updateChoice action
                |> swap
                |> Nest.mapItems (Tuple.mapSecond <| map <| always A.Stay)
                |> (Choice focus shape)
        Group focus shape control ->
            control
                |> Nest.updateGroup action
                |> swap
                |> Nest.mapItems (Tuple.mapSecond <| map <| always A.Stay)
                |> (Group focus shape)
        Live innerProp -> innerProp |> update action |> set A.Fire |> Live
