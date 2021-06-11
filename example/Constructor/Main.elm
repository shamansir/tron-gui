module Constructor.Main exposing (..)


import Browser exposing (Document)

import Tron exposing (Tron)
import Tron.OfValue as OfValue
import Tron.Builder.Unit as Tron
import Tron.Option as Option
import Tron.Style.Dock as Dock
import Tron.Style.Theme as Theme
import Tron.Path as Path exposing (Path)

import Tron.Core as Tron
import Tron.Property as Property
import Tron.Control.Nest as Nest

import WithTron exposing (ProgramWithTron)

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html


type alias Model =
    ( Path
    , Tron ()
    )


type Msg
    = NoOp
    | AddButton


for : Model -> OfValue.Tron Msg
for =
    Tuple.second
    >> Tron.map (always NoOp)
    >> OfValue.lift


init : Model
init =
    ( Path.start
    , Tron.root []
    )


update : Msg -> Model -> Model
update msg ( path, currentGui ) =
    case msg of
        NoOp ->
            ( path, currentGui )
        AddButton ->
            ( path
            , currentGui
                |> Property.updateAt path
                    (Property.append
                        ( "test"
                        , Tron.button
                        )
                    )
            )


view : Model -> Html Msg
view _ =
    Html.div
        []
        [ Html.button
            [ Html.onClick AddButton
            , Html.value "+"
            ]
            [ Html.text "+" ]
        ]


-- subscriptions : Model -> Sub Msg
-- subscriptions _ = Sub.none


main : ProgramWithTron () Model Msg
main =
    WithTron.sandbox
        (Option.toHtml Dock.bottomCenter Theme.dark)
        Option.noCommunication
        { for = for
        , init = init
        , view = view
        , update = update
        --, subscriptions = subscriptions
        }