module BinPack exposing (..)


import Random
import Bounds exposing (Bounds)


{- Based on: https://github.com/bflyblue/binpack/blob/master/Data/BinPack/R2.hs -}


type BinPack a
    = Node

        { width : Float
        , height : Float
        }

        { right : BinPack a
        , below : BinPack a
        }

        a

    | Free

        { width : Float
        , height : Float
        }


map : ( a -> b ) -> BinPack a -> BinPack b
map f bp =
    case bp of
        Node size { right, below } v ->
            Node size
                { right = map f right
                , below = map f below
                }
                (f v)
        Free size -> Free size


fold : ( a -> b -> b ) -> b -> BinPack a -> b
fold f =
    fold1
        (\bp prev ->
            case bp of
                Node _ _ v -> f v prev
                Free _ -> prev
        )


fold1 : ( BinPack a -> b -> b ) -> b -> BinPack a -> b
fold1 f i bp =
    case bp of
        Node _ { right, below } _ ->
            let
                current = f bp i
                fromRight = fold1 f current right
                fromBelow = fold1 f fromRight below
            in fromBelow
        Free _ -> f bp i


unfold : ( ( a, Bounds ) -> k -> k ) -> k -> BinPack a -> k
unfold f =
    let
        helper x y v bp =
           case bp of
               Free _ -> v
               Node r n i ->
                   f ( i,
                        { x = x, y = y
                        , width = r.width
                        , height = r.height
                        }
                      )
                    <| helper x (y + r.height) (helper (x + r.width) y v n.right) n.below
    in helper 0 0


unfold1 : ( ( BinPack a, Bounds ) -> k -> k ) -> k -> BinPack a -> k
unfold1 f =
    let
        helper x y v bp =
           case bp of
               Free r ->
                    f ( bp,
                        { x = x, y = y
                        , width = r.width
                        , height = r.height
                        }
                      )
                    v
               Node r n i ->
                   f ( bp,
                        { x = x, y = y
                        , width = r.width
                        , height = r.height
                        }
                      )
                    <| helper x (y + r.height) (helper (x + r.width) y v n.right) n.below
    in helper 0 0


unpack : BinPack a -> List (a, Bounds)
unpack = unfold (::) []


unpack1 : BinPack a -> List (BinPack a, Bounds)
unpack1 = unfold1 (::) []


container w h = Free { width = w, height = h }


node w h r b a =
    Node
        { width = w
        , height = h
        }
        { right = r
        , below = b
        }
        a


pack : ( { width : Float, height : Float }, a ) -> BinPack a -> Maybe (BinPack a)
pack ( rect, value ) model =
    case model of
        Free f ->
            if rect.width <= f.width && rect.height <= f.height
            then
                Just
                    <| let
                        pright = container (f.width - rect.width) rect.height
                        pbelow = container f.width (f.height - rect.height)
                    in node rect.width rect.height pright pbelow value
            else Nothing
        Node r n nodeValue ->
            case pack ( rect, value ) n.right of
                Just newRight ->
                    Just <| node r.width r.height newRight n.below nodeValue
                Nothing ->
                    case pack ( rect, value ) n.below of
                        Just newBelow ->
                            Just <| node r.width r.height n.right newBelow nodeValue
                        Nothing -> Nothing


pack1 : ( { width : Float, height : Float }, a ) -> BinPack a -> BinPack a
pack1 ( rect, value ) model =
    pack ( rect, value ) model |> Maybe.withDefault model


find : { x : Float, y : Float } -> BinPack a -> Maybe ( a, Bounds )
find pos =
    unfold
        (\ ( v, bounds ) foundBefore ->
            case foundBefore of
                Just _ -> foundBefore
                Nothing ->
                    if (pos.x >= bounds.x)
                    && (pos.y >= bounds.y)
                    && (pos.x < bounds.x + bounds.width)
                    && (pos.y < bounds.y + bounds.height)
                        then Just ( v, bounds )
                        else Nothing
        )
        Nothing
