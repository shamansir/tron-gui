module Tron.Control.Impl.Nest exposing (..)


import Array exposing (Array)
import Array.Extra as Array

import Tron.Control as Core exposing (Control)
import Tron.Control.Impl.Button as Button
import Tron.Control.Action as A
import Tron.Pages exposing (PageNum)
import Tron.Util as Util


type Form
    = Expanded
    | Collapsed
    | Detached


type ChoiceMode -- FIXME: can't be `Expanded` and `Knob` / `SwitchThrough` at the same time
    = Pages
    | SwitchThrough
    | Knob


type alias ItemId = Int


type alias NestControl item value a
    = Core.Control
        ( Array item )
        value
        a


type alias GroupControl item a
    = NestControl
        item
        { form : Form
        , face : Maybe Button.Face
        , page : PageNum
        }
        a


type alias ChoiceControl item a =
    NestControl
        item
        { form : Form
        , face : Maybe Button.Face
        , selected : ItemId
        , prevSelected : Maybe ItemId -- FIXME: needed only for `Knob`
        , page : PageNum
        , mode : ChoiceMode
        }
        a


type alias Transient =
    { form : Form
    , page : PageNum
    }


createGroup : ( Array item ) -> a -> GroupControl item a
createGroup items =
    Core.Control
        items
        { form = Collapsed
        , face = Nothing
        , page = 0
        }


createChoice : ( Array item ) -> a -> ChoiceControl item a
createChoice items =
    Core.Control
        items
        { form = Collapsed
        , face = Nothing
        , selected = 0
        , prevSelected = Nothing
        , page = 0
        , mode = Pages
        }


updateGroup : A.Action -> GroupControl item a -> ( GroupControl item a, A.Change )
updateGroup action control =
    case action of
        A.Execute ->
            ( toggle control, A.Fire )
        _ ->
            ( control, A.None )


updateChoice : A.Action -> ChoiceControl item a -> ( ChoiceControl item a, A.Change )
updateChoice action control =
    case action of
        A.Execute ->
            ( case getChoiceMode control of
                Pages ->
                    toggle control
                Knob ->
                    toNext control
                SwitchThrough ->
                    toNext control
            , A.Fire
            )
        A.Select n ->
            ( select n control
            , A.Fire
            )
        -- TODO: dragging
        _ ->
            ( control, A.None )


-- updateChoiceItem : A.Action -> ( ChoiceControl item a, Index ) -> ( ChoiceControl item a, A.Change )
-- updateChoiceItem


get : ItemId -> NestControl item value a -> Maybe item
get n = getItems >> Array.get n


find : comparable -> NestControl (comparable, item) value a -> Maybe ( ItemId, item )
find what =
    getItems
        >> Array.indexedMap Tuple.pair
        >> Array.findMap
            (\(index, (label, item)) ->
                if label == what
                    then Just (index, item)
                    else Nothing
            )


select : ItemId -> ChoiceControl item a -> ChoiceControl item a
select index (Core.Control setup state handler) =
    Core.Control setup { state | selected = index } handler


getSelected : ChoiceControl item a -> Maybe item
getSelected control =
    get (whichSelected control) control


isSelected : ChoiceControl item a -> ItemId -> Bool
isSelected control n = whichSelected control == n


whichSelected : ChoiceControl item a -> ItemId
whichSelected (Core.Control _ { selected } handler) = selected


expand
     : Core.Control
            setup
            { r | form : Form }
            a
    -> Core.Control
            setup
            { r | form : Form }
            a
expand (Core.Control setup state handler) =
    Core.Control setup { state | form = Expanded } handler


collapse
     : Core.Control
            setup
            { r | form : Form }
            a
    -> Core.Control
            setup
            { r | form : Form }
            a
collapse (Core.Control setup state handler) =
    Core.Control setup { state | form = Collapsed } handler


detach
     : Core.Control
            setup
            { r | form : Form }
            a
    -> Core.Control
            setup
            { r | form : Form }
            a
detach (Core.Control setup state handler) =
    Core.Control setup { state | form = Detached } handler


toggle
     : Core.Control
            setup
            { r | form : Form }
            a
    -> Core.Control
            setup
            { r | form : Form }
            a
toggle (Core.Control setup state handler) =
    Core.Control
        setup
        { state
        | form =
            case state.form of
                Expanded -> Collapsed
                Collapsed -> Expanded
                Detached -> Detached
        }
        handler


getForm : Core.Control setup { r | form : Form } a -> Form
getForm (Core.Control _ { form } _) = form


is : Form -> Core.Control setup { r | form : Form } a -> Bool
is checkedForm (Core.Control _ { form } _) = checkedForm == form


getChoiceMode : Core.Control setup { r | mode : ChoiceMode } a -> ChoiceMode
getChoiceMode (Core.Control _ { mode } _) = mode


toNext : ChoiceControl item a -> ChoiceControl item a
toNext (Core.Control items value a) =
    Core.Control
        items
        { value
        | selected =
            if value.selected >= Array.length items - 1 then
                0
            else
                value.selected + 1
        , prevSelected = Nothing
        }
        a


setChoiceMode : ChoiceMode -> Core.Control setup { r | mode : ChoiceMode } a -> Core.Control setup { r | mode : ChoiceMode } a
setChoiceMode newMode (Core.Control setup value a) =
    Core.Control
        setup
        { value | mode = newMode }
        a


getItems : NestControl item value a -> Array item
getItems (Core.Control items _ _) = items


setItems
     : Array item
    -> Core.Control ( Array item ) value a
    -> Core.Control ( Array item ) value a
setItems newItems (Core.Control _ value handler) =
    Core.Control newItems value handler


mapItems
     : (itemA -> itemB)
    -> NestControl
            itemA
            value
            a
    -> NestControl
            itemB
            value
            a
mapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.map f ) value handler


indexedMapItems
     : (Int -> itemA -> itemB)
    -> NestControl
            itemA
            value
            a
    -> NestControl
            itemB
            value
            a
indexedMapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.indexedMap f ) value handler


withItem
     : Int
    -> (item -> item)
    -> NestControl item value a
    -> NestControl item value a
withItem id f ( Core.Control items state handler ) =
    Core.Control
        ( case Array.get id items of
            Just item ->
                items
                |> Array.set id (f item)
            Nothing -> items
        )
        state
        handler


withItemAt
     : comparable
    -> (item -> item)
    -> NestControl ( comparable, item ) value a
    -> NestControl ( comparable, item ) value a
withItemAt label f (( Core.Control items state handler ) as control) =
    Core.Control
        ( case find label control of
            Just ( id, item ) ->
                items
                |> Array.set id (label, f item)
            Nothing -> items
        )
        state
        handler


getPage : Core.Control setup { r | page : PageNum } a -> PageNum
getPage (Core.Control _ { page } _) = page


switchTo
    :  PageNum
    -> Core.Control setup { r | page : PageNum } a
    -> Core.Control setup { r | page : PageNum } a
switchTo pageNum (Core.Control setup state hanlder) =
    Core.Control
        setup
        { state | page = pageNum }
        hanlder


getTransientState
    : Core.Control
        setup
        { r | form : Form
        , page : PageNum
        }
        a
    -> Transient
getTransientState (Core.Control _ state _) =
    { form = state.form
    , page = state.page
    }


restoreTransientState
    :  Core.Control
        setup
        { r
        | form : Form
        , page : PageNum
        }
        a
    -> Transient
    -> Core.Control
        setup
        { r
        | form : Form
        , page : PageNum
        }
        a
restoreTransientState (Core.Control setup state handler) src =
    Core.Control
        setup
        { state
        | form = src.form
        , page = src.page
        }
        handler


setFace
     : Button.Face
    -> Core.Control
            setup
            { r | face : Maybe Button.Face }
            a
    -> Core.Control
            setup
            { r | face : Maybe Button.Face }
            a
setFace face (Core.Control setup state handler) =
    Core.Control setup { state | face = Just face } handler


clearFace
     : Core.Control
            setup
            { r | face : Maybe Button.Face }
            a
    -> Core.Control
            setup
            { r | face : Maybe Button.Face }
            a
clearFace (Core.Control setup state handler) =
    Core.Control setup { state | face = Nothing } handler


toChoice : GroupControl item a -> ChoiceControl item a
toChoice (Core.Control items { form, page, face } a) =
    Core.Control
        items
        { form = form
        , page = page
        , face = face
        , selected = 0
        , prevSelected = Nothing
        , mode = Pages
        }
        a


append : item -> NestControl item value a -> NestControl item value a
append what (Core.Control items value a) =
    Core.Control
        (Array.append items <| Array.repeat 1 what)
        value
        a


remove : ItemId -> NestControl item value a -> NestControl item  value a
remove id (Core.Control items value a) =
    Core.Control
        (Array.append
            (items |> Array.slice 0 id)
            (items |> Array.slice (id + 1) (Array.length items) )
        )
        value
        a


forward : ItemId -> NestControl item value a -> NestControl item  value a
forward id (Core.Control items value a) =
    if (id >= Array.length items - 1) then Core.Control items value a
    else
        Core.Control
            (Array.append
                (items |> Array.slice 0 id) -- all before item
                (Array.append
                    (items |> Array.slice (id + 1) (id + 2)) -- item after the current
                    (Array.append
                        (items |> Array.slice id (id + 1)) -- current item
                        (items |> Array.slice (id + 2) (Array.length items)) -- everything else
                    )
                )
            )
            value
            a


backward : ItemId -> NestControl item value a -> NestControl item  value a
backward id (Core.Control items value a) =
    if (id <= 0) then Core.Control items value a
    else
        Core.Control
            (Array.append
                (items |> Array.slice 0 (id - 1)) -- before item, one less
                (Array.append
                    (items |> Array.slice id (id + 1)) -- current item
                    (Array.append
                        (items |> Array.slice (id - 1) id) -- item after the current
                        (items |> Array.slice (id + 1) (Array.length items)) -- everything else
                    )
                )
            )
            value
            a