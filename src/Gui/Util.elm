module Gui.Util exposing (..)


import Gui.Control exposing (..)
import Gui.Mouse exposing (..)


align : Float -> Float
align v =
    if v > 1 then 1.0
    else if v < 0 then 0.0
        else v


applyKnobMove : MouseState -> MouseState -> Float -> AlterKnob
applyKnobMove prev next curValue =
    case next.dragFrom of
        Just dragFrom ->
            if next.pos /= dragFrom then
                let
                    originY = dragFrom.y
                    curY = next.pos.y
                    topY = toFloat originY + (knobDistance / 2)
                    diffY = (topY - toFloat curY) / knobDistance
                in
                    Alter <| align diffY
            else Stay
        _ -> Stay


applyXYMove : MouseState -> MouseState -> ( Float, Float ) -> AlterXY
applyXYMove prev next curValue =
    case next.dragFrom of
        Just dragFrom ->
            if next.pos /= dragFrom then
                let
                    originX = dragFrom.x
                    originY = dragFrom.y
                    curX = next.pos.x
                    curY = next.pos.y
                    leftX = toFloat originX - (knobDistance / 2)
                    -- Y is going from top to bottom
                    topY = toFloat originY + (knobDistance / 2)
                    diffX = (toFloat curX - leftX) / knobDistance
                    diffY = (topY - toFloat curY) / knobDistance
                in
                    Alter_
                        ( align diffX
                        , align (1 - diffY)
                        )
            else Stay_
        _ -> Stay_


findMap : (a -> Maybe x) -> List a -> Maybe x
findMap toValue =  -- TODO: re-use in code where such `foldl` is used
    List.foldl
        (\item maybeResult ->
            case maybeResult of
                Nothing -> toValue item
                _ -> maybeResult
        )
        Nothing


alterKnob : Axis -> AlterKnob -> Float -> Float
alterKnob { min, max, step } alter curValue =
    case alter of
        Alter amount ->
            -- amount is a (0 <= value <= 1)
            min + (amount * (max - min)) -- TODO: apply step
        Stay -> curValue


alterXY : ( Axis, Axis ) -> AlterXY -> ( Float, Float ) -> ( Float, Float )
alterXY ( xSpec, ySpec ) alter ( curX, curY ) =
    case alter of
        Alter_ ( amountX, amountY ) ->
            -- amount is a (0 <= value <= 1)
            ( xSpec.min + (amountX * (xSpec.max - xSpec.min)) -- TODO: apply step
            , ySpec.min + (amountY * (ySpec.max - ySpec.min))
            )
        Stay_ -> ( curX, curY )
