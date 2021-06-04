module Tron.Pages exposing (..)


import Array exposing (Array)

--import Tron.Control.Nest as Nest exposing (..)

import Size exposing (..)


type alias PageNum = Int


type alias Count = Int


type Pages a = Pages PageNum a (List a) -- i.e. NonEmpty array


single : a -> Pages a
single v = Pages 0 v []


map : (a -> b) -> Pages a -> Pages b
map f (Pages num fst others) =
    Pages num (f fst) <| List.map f <| others


fold : (a -> b -> b) -> b -> Pages a -> b
fold f def =
    -- also : List.foldl f (f fst def) others
    toList >> List.foldl f def


toList : Pages a -> List a
toList (Pages _ fst other) = fst :: other


switchTo : PageNum -> Pages a -> Pages a
switchTo newPage (Pages _ fst other) =
    Pages newPage fst other


getCurrentNum : Pages a -> PageNum
getCurrentNum (Pages num _ _) = num


getCurrent : Pages a -> Maybe a
getCurrent pages =
    get (getCurrentNum pages) pages


get : PageNum -> Pages a -> Maybe a
get num (Pages _ fst other) =
    case num of
        0 -> Just fst
        n ->
            Array.fromList other
                |> Array.get (n - 1)


create : a -> List a -> Pages a
create = Pages 0


fromList : List a -> Maybe (Pages a)
fromList list =
    case list of
        [] -> Nothing
        first::others -> Just <| Pages 0 first others


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


distribute : Int -> List a -> Pages (List a)
distribute maxItems = distributeBy (\list _ -> List.length list < maxItems)


distributeOver : Count -> List a -> Pages (List a)
distributeOver pagesCount all =
    let
        maxItems =
            if List.length all // pagesCount * pagesCount == List.length all - 1
            then List.length all // pagesCount
            else (List.length all // pagesCount) + 1
    in all |> distributeBy (\list _ -> List.length list <= maxItems)


count : Pages a -> Count
count (Pages _ _ list) = List.length list + 1


-- put all the items on the first page
disable : Pages (List a) -> Pages (List a)
disable (Pages _ first other) =
    Pages 0 (List.concat <| first::other) []
