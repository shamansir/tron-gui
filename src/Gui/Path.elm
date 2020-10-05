module Gui.Path exposing (..)


type Path = Path (List Int)


start : Path
start = Path []


toList : Path -> List Int
toList (Path l) = l


fromList : List Int -> Path
fromList = Path


advance : Int -> Path -> Path
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


pop : Path -> Maybe ( Path, Int )
pop (Path l) =
    let
        reversed = List.reverse l
    in
        List.head reversed
            |> Maybe.map
                (\deepest ->
                    ( List.tail reversed
                        |> Maybe.withDefault []
                        |> fromList
                    , deepest
                    )
                )


toString : Path -> String
toString (Path list) =
    list
        |> List.map String.fromInt
        |> String.join "/"


howDeep : Path -> Int
howDeep (Path list) = List.length list


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

