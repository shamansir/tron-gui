module Tron.Path exposing (..)


type alias Label = String


type alias Index = Int


type Path = Path (List (Index, Label))


start : Path
start = Path []


length : Path -> Int
length (Path l) = List.length l


toList : Path -> List (Index, Label)
toList (Path l) = l


fromList : List (Index, Label) -> Path
fromList = Path


advance : ( Index, Label ) -> Path -> Path
advance n (Path l) = Path <| l ++ [ n ]


retract : Path -> Path
retract =
    pop
        >> Maybe.map Tuple.first
        >> Maybe.withDefault start


head : Path -> Maybe Path
head (Path list) =
    List.head list
        |> Maybe.map List.singleton
        |> Maybe.map Path


equal : Path -> Path -> Bool
equal (Path p1) (Path p2) = p1 == p2


-- deepest : Path -> Maybe Int
-- deepest (Path l) =
--     List.reverse l
--         |> List.head


pop : Path -> Maybe ( Path, ( Index, Label ) )
pop (Path l) =
    case List.reverse l of
        [] ->
            Nothing

        last_ :: rest ->
            ( Path <| List.reverse rest, last_ )
                |> Just


reverse : Path -> Path
reverse (Path l) = Path <| List.reverse l


last : Path -> Maybe ( Index, Label )
last =
    pop >> Maybe.map Tuple.second


lastIndex : Path -> Maybe Index
lastIndex = last >> Maybe.map Tuple.first


lastLabel : Path -> Maybe Label
lastLabel = last >> Maybe.map Tuple.second


toString : Path -> String
toString (Path list) =
    list
        |> List.map (\(idx, label) -> String.fromInt idx ++ ":" ++ label)
        |> String.join "/"


howDeep : Path -> Int
howDeep = length


isRoot : Path -> Bool
isRoot path = howDeep path == 0


add : Path -> Path -> Path
add (Path a) (Path b) = Path <| a ++ b


sub : Path -> Path -> Path
sub (Path w) (Path f) =
    let
        helper what from =
            case (what, from) of
                ( [], ys ) -> ys
                ( xs, [] ) -> []
                ( x::xs, y::ys ) ->
                    if x == y then
                        helper xs ys
                    else from
    in
        Path <| helper w f


toIndexPath : Path -> List Index
toIndexPath (Path list) = list |> List.map Tuple.first


toLabelPath : Path -> List Label
toLabelPath (Path list) = list |> List.map Tuple.second


walk : (Int -> (Index, Label) -> a) -> Path -> List a
walk f (Path l) = List.indexedMap f l


update : (Int -> (Index, Label) -> (Index, Label)) -> Path -> Path
update f = Path << walk f