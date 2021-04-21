module Tron.Control.Nest exposing (..)


import Array exposing (Array)

import Tron.Control as Core exposing (Control)
import Tron.Control.Button as Button
import Tron.Pages exposing (PageNum)


type Form
    = Expanded
    | Collapsed
    | Detached
    -- TODO: | ChoiceToggle


type alias ItemId = Int


type alias GroupControl item msg =
    Core.Control
        ( Array item )
        { form : Form
        , face : Maybe Button.Face
        , page : PageNum
        }
        msg


type alias ChoiceControl item msg =
    Core.Control
        ( Array item )
        { form : Form
        , selected : ItemId
        , page : PageNum
        }
        msg


type alias Transient =
    { form : Form
    , page : PageNum
    }


get : ItemId -> Core.Control ( Array item ) value msg -> Maybe item
get n = getItems >> Array.get n


select : ItemId -> ChoiceControl item msg -> ChoiceControl item msg
select index (Core.Control setup state handler) =
    Core.Control setup { state | selected = index } handler


getSelected : ChoiceControl item msg -> Maybe item
getSelected control =
    get (whichSelected control) control


isSelected : ChoiceControl item msg -> ItemId -> Bool
isSelected control n = whichSelected control == n


whichSelected : ChoiceControl item msg -> ItemId
whichSelected (Core.Control _ { selected } handler) = selected


expand
     : Core.Control
            setup
            { a | form : Form }
            msg
    -> Core.Control
            setup
            { a | form : Form }
            msg
expand (Core.Control setup state handler) =
    Core.Control setup { state | form = Expanded } handler


collapse
     : Core.Control
            setup
            { a | form : Form }
            msg
    -> Core.Control
            setup
            { a | form : Form }
            msg
collapse (Core.Control setup state handler) =
    Core.Control setup { state | form = Collapsed } handler


detach
     : Core.Control
            setup
            { a | form : Form }
            msg
    -> Core.Control
            setup
            { a | form : Form }
            msg
detach (Core.Control setup state handler) =
    Core.Control setup { state | form = Detached } handler


toggle
     : Core.Control
            setup
            { a | form : Form }
            msg
    -> Core.Control
            setup
            { a | form : Form }
            msg
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


getForm : Core.Control setup { a | form : Form } msg -> Form
getForm (Core.Control _ { form } _) = form


is : Form -> Core.Control setup { a | form : Form } msg -> Bool
is checkedForm (Core.Control _ { form } _) = checkedForm == form


getItems : Core.Control ( Array item ) value msg -> Array item
getItems (Core.Control items _ _) = items


setItems
     : Array item
    -> Core.Control ( Array item ) value msg
    -> Core.Control ( Array item ) value msg
setItems newItems (Core.Control _ value handler) =
    Core.Control newItems value handler


mapItems
     : (itemA -> itemB)
    -> Core.Control
            ( Array itemA )
            value
            msg
    -> Core.Control
            ( Array itemB )
            value
            msg
mapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.map f ) value handler


indexedMapItems
     : (Int -> itemA -> itemB)
    -> Core.Control
            ( Array itemA )
            value
            msg
    -> Core.Control
            ( Array itemB )
            value
            msg
indexedMapItems f (Core.Control items value handler) =
    Core.Control ( items |> Array.indexedMap f ) value handler


withItem
     : Int
    -> (item -> item)
    -> Core.Control (Array item) value msg
    -> Core.Control (Array item) value msg
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

getPage : Core.Control setup { a | page : PageNum } msg -> PageNum
getPage (Core.Control _ { page } _) = page


switchTo
    :  PageNum
    -> Core.Control setup { a | page : PageNum } msg
    -> Core.Control setup { a | page : PageNum } msg
switchTo pageNum (Core.Control setup state hanlder) =
    Core.Control
        setup
        { state | page = pageNum }
        hanlder


getTransientState
    : Core.Control
        setup
        { a | form : Form
        , page : PageNum
        }
        msg
    -> Transient
getTransientState (Core.Control _ state _) =
    { form = state.form
    , page = state.page
    }


restoreTransientState
    :  Core.Control
        setup
        { a
        | form : Form
        , page : PageNum
        }
        msg
    -> Transient
    -> Core.Control
        setup
        { a
        | form : Form
        , page : PageNum
        }
        msg
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
            { a | face : Maybe Button.Face }
            msg
    -> Core.Control
            setup
            { a | face : Maybe Button.Face }
            msg
setFace face (Core.Control setup state handler) =
    Core.Control setup { state | face = Just face } handler


clearFace
     : Core.Control
            setup
            { a | face : Maybe Button.Face }
            msg
    -> Core.Control
            setup
            { a | face : Maybe Button.Face }
            msg
clearFace (Core.Control setup state handler) =
    Core.Control setup { state | face = Nothing } handler


toChoice : (ItemId -> msg) -> GroupControl item msg -> ChoiceControl item msg
toChoice userHandler (Core.Control items { form, page } handler) =
    Core.Control
        items
        { form = form
        , page = page
        , selected = 0
        }
        (Just <| .selected >> userHandler)
