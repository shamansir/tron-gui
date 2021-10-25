module Example.Goose.Main exposing (init, view, update)


import Color
import Svg exposing (Svg)

import Example.Goose.Model exposing (..)
import Example.Goose.Model as Model
import Example.Goose.View as V
import Example.Goose.Msg exposing (..)


init : Model
init =
    Model.default


view : Model -> Svg msg
view = V.view


update : Msg -> Model -> Model
update msg model =
    let
        changeHonk f =
            { model
            | honk = model.honk |> Tuple.mapSecond f
            }
        changeColor f =
            { model | colors = f model.colors }
        changeEye f =
            { model | eye = f model.eye }
    in case msg of
        NoOp ->
            model
        PunkOn ->
            { model | punk = True }
        PunkOff ->
            { model | punk = False }
        HonkOn ->
            { model | honk = ( True, Tuple.second model.honk ) }
        HonkOff ->
            { model | honk = ( False, Tuple.second model.honk ) }
        ChangeHonkText text ->
            changeHonk (\honk -> { honk | text = text })
        ChangeHonkTextSize size ->
            changeHonk (\honk -> { honk | size = size })
        ChangeHonkTextPosition pos ->
            changeHonk (\honk -> { honk | position = pos })
        ChangeHonkTextColor color ->
            changeHonk (\honk -> { honk | color = color })
        ChangeEyePosition pos ->
            changeEye (\eye -> { eye | position = pos })
        ChangeEyeSize size ->
            changeEye (\eye -> { eye | size = size })
        LookAt direction ->
            { model | lookAt = direction }
        ChangeShoes shoes ->
            { model | shoes = shoes }
        ChangeFeathersColor color ->
            changeColor (\colors -> { colors | feathers = color })
        ChangeSkinColor color ->
            changeColor (\colors -> { colors | skin = color })
        ChangeEyeColor color ->
            changeColor (\colors -> { colors | eye = color })
        ChangeIroquoisColor color ->
            changeColor (\colors -> { colors | iroquois = color })
        ChangeBackground color ->
            changeColor (\colors -> { colors | background = color })
