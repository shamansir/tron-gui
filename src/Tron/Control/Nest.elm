module Tron.Control.Nest exposing (..)


import Array exposing (Array)

import Tron.Control as Core exposing (Control)
import Tron.Control.Button as Button
import Tron.Pages exposing (PageNum)


type Form
    = Expanded
    | Collapsed
    | Detached
    | SwitchThrough
    | Knob


type alias ItemId = Int


type alias GroupControl item a =
    Core.Control
        ( Array item )
        { form : Form
        , face : Maybe Button.Face
        , page : PageNum
        }
        a


type alias ChoiceControl item a =
    Core.Control
        ( Array item )
        { form : Form
        , face : Maybe Button.Face
        , selected : ItemId
        , prevSelected : Maybe ItemId -- FIXME: needed only for `Knob`
        , page : PageNum
        }
        a


type alias Transient =
    { form : Form
    , page : PageNum
    }


get : ItemId -> Core.Control ( Array item ) value a -> Maybe item
get n = getItems >> Array.get n


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
                SwitchThrough -> SwitchThrough -- toggle is performed another way in Core
                Knob -> Knob -- toggle is performed another way in Core
        }
        handler


getForm : Core.Control setup { r | form : Form } a -> Form
getForm (Core.Control _ { form } _) = form


is : Form -> Core.Control setup { r | form : Form } a -> Bool
is checkedForm (Core.Control _ { form } _) = checkedForm == form


setForm : Form -> Core.Control setup { r | form : Form } a -> Core.Control setup { r | form : Form } a
setForm form (Core.Control setup value a) = Core.Control setup { value | form = form } a


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


getItems : Core.Control ( Array item ) value a -> Array item
getItems (Core.Control items _ _) = items


setItems
     : Array item
    -> Core.Control ( Array item ) value a
    -> Core.Control ( Array item ) value a
setItems newItems (Core.Control _ value handler) =
    Core.Control newItems value handler


mapItems
     : (itemA -> itemB)
    -> Core.Control
            ( Array itemA )
            value
            a
    -> Core.Control
            ( Array itemB )
            value
            a
mapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.map f ) value handler


indexedMapItems
     : (Int -> itemA -> itemB)
    -> Core.Control
            ( Array itemA )
            value
            a
    -> Core.Control
            ( Array itemB )
            value
            a
indexedMapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.indexedMap f ) value handler


withItem
     : Int
    -> (item -> item)
    -> Core.Control (Array item) value a
    -> Core.Control (Array item) value a
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
        }
        a
