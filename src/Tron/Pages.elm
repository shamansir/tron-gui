module Tron.Pages exposing (..)


import Array exposing (Array)

--import Tron.Control.Impl.Nest as Nest exposing (..)

import Size exposing (..)



{-| -}
type PageRef
    = Last
    | AtFocus
    | Page_ Int
    -- | Nonsence


type Pages a = Pages PageRef a (List a) -- i.e. NonEmpty array


type Page = Page Int


type Count = Count Int


type Maximum = Maximum Int


type Selected = Selected Int


first : PageRef
first = at <| page 0


last : PageRef
last = Last


current : PageRef
current = AtFocus


selFirst : Selected
selFirst = Selected 0


at : Page -> PageRef
at (Page n) = Page_ n


page : Int -> Page
page = Page


single : a -> Pages a
single v = Pages (Page_ 0) v []


map : (a -> b) -> Pages a -> Pages b
map f (Pages num fst others) =
    Pages num (f fst) <| List.map f <| others


fold : (a -> b -> b) -> b -> Pages a -> b
fold f def =
    -- also : List.foldl f (f fst def) others
    toList >> List.foldl f def


toList : Pages a -> List a
toList (Pages _ fst other) = fst :: other


switchTo : PageRef -> Pages a -> Pages a
switchTo newPage (Pages _ fst other) =
    Pages newPage fst other


getCurrentPage : Pages a -> Maybe Page
getCurrentPage (Pages ref _ others) =
    case ref of
        Page_ n -> Just <| Page n
        Last -> Just <| Page <| List.length others
        AtFocus -> Nothing


getCurrentRef : Pages a -> PageRef
getCurrentRef (Pages ref _ _) = ref


getCurrent : Selected -> Pages (List a) -> Maybe (List a)
getCurrent selected pages =
    getAt selected (getCurrentRef pages) pages



whereIs : Selected -> Pages (List a) -> Maybe Page
whereIs (Selected idx) (Pages _ fst other) =
    (fst :: other)
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\(pageNum, pageList) ( index, foundBefore ) ->
                ( index - List.length pageList
                , case foundBefore of
                    Just _ ->
                        foundBefore
                    Nothing ->
                        if index < List.length pageList then
                            Just pageNum
                        else
                            Nothing
                )
            )
            ( idx, Nothing )
        |> Tuple.second
        |> Maybe.map Page


getAt : Selected -> PageRef -> Pages (List a) -> Maybe (List a)
getAt selected ref (Pages _ fst other as pages) =
    case ref of
        Page_ 0 -> Just fst
        Page_ n ->
            Array.fromList other
                |> Array.get (n - 1)
        Last ->
            if List.length other > 0 then
                Array.fromList other
                    |> Array.get (List.length other - 1)
            else
                Just fst
        AtFocus ->
            pages
                |> whereIs selected
                |> Maybe.andThen
                    (\page_ ->
                        getAt selected (at page_) pages
                    )


get : Page -> Pages a -> Maybe a
get (Page num) (Pages _ fst other) =
    case num of
        0 -> Just fst
        n ->
            Array.fromList other
                |> Array.get (n - 1)


create : a -> List a -> Pages a
create = Pages first


fromList : List a -> Maybe (Pages a)
fromList list =
    case list of
        [] -> Nothing
        first_::others -> Just <| Pages first first_ others


distributeBy : (List a -> a -> Bool) -> List a -> Pages (List a)
distributeBy fits =
    List.foldl
        (\item pages ->
            case pages of
                lastPage::prevPages ->
                    if item |> fits lastPage then
                        (item::lastPage)::prevPages
                    else
                        [item]::lastPage::prevPages
                [] ->
                    [[item]]
        )
        [[]]
    >> List.map List.reverse
    >> List.reverse
    >> fromList
    >> Maybe.withDefault (create [] [[]])


distribute : Maximum -> List a -> Pages (List a)
distribute (Maximum maxItems) = distributeBy (\list _ -> List.length list < maxItems)


-- distributeInShape : ( Maximum, Maximum ) -> List a -> Pages (List a)
-- distributeInShape _ _ = create [] [] -- TODO


distributeOver : Count -> List a -> ( Int, Pages (List a) )
distributeOver (Count pagesCount) all =
    if (pagesCount <= 0) then ( 0, single [] )
    else
        let
            maxItems =
                if List.length all // pagesCount * pagesCount == List.length all - 1
                then List.length all // pagesCount
                else (List.length all // pagesCount) + 1
        in
            ( maxItems
            , all |> distributeBy (\list _ -> List.length list <= maxItems)
            )


count : Pages a -> Count
count (Pages _ _ list) =
    Count <| List.length list + 1


-- put all the items on the first page
disable : Pages (List a) -> Pages (List a)
disable (Pages _ first_ other) =
    Pages first (List.concat <| first_::other) []


{-
refToNum : PageRef -> Int
refToNum ref =
    case ref of
        Last -> -1
        AtFocus -> -2
        Page_ n -> n
-}