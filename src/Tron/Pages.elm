module Tron.Pages exposing (..)


import Array exposing (Array)

--import Tron.Control.Nest as Nest exposing (..)

import Size exposing (..)


type alias PageNum = Int


type alias Count = Int


type Pages a = Pages PageNum a (List a) -- i.e. NonEmpty array


map : (a -> b) -> Pages a  -> Pages b
map f (Pages num fst others) =
    Pages num (f fst) <| List.map f <| others


toList : Pages a -> List a
toList (Pages _ fst other) = fst :: other


switchTo : PageNum -> Pages a -> Pages a
switchTo newPage (Pages _ fst other) =
    Pages newPage fst other


getCurrentNum : Pages a -> PageNum
getCurrentNum (Pages num _ _) = num


getCurrent : Pages a -> Maybe a
getCurrent (Pages num fst other) =
    case num of
        0 -> Just fst
        n ->
            Array.fromList other
                |> Array.get n


get : PageNum -> Maybe a
get pageNum = Nothing


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
                        if item |> fits lastPage  then
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
distribute maxItems = distributeBy (\list _ -> (List.length list + 1) <= maxItems)


count : Pages a -> Count
count (Pages _ _ list) = List.length list + 1
