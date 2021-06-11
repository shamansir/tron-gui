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
import Tron.Property as Property exposing (LabelPath)
import Tron.Control.Nest as Nest
import Tron.Control.Value as V exposing (Value)
import Tron.Expose.Convert as Property

import WithTron exposing (ProgramWithTron)

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html


type Type
    = Waiting
    | None
    | Knob
    | XY
    | Color
    | Text
    | Toggle
    | Button
    | Choice
    | Group


type alias Model =
    (
        { target : Path
        , current : Maybe (Tron Def)
        }
    , Tron ()
    )


type alias Def = ( ( Path, LabelPath ), Type )


type Msg
    = NoOp
    | Add (Tron ())
    | Edit (Tron Def)
    | ShowMenu


for : Model -> OfValue.Tron Msg
for =
    Tuple.second
    >> Tron.map (always NoOp)
    >> OfValue.lift


init : Model
init =
    (
        { target = Path.start
        , current = Nothing
        }
    , Tron.root []
    )


update : Msg -> Model -> Model
update msg ( state, currentGui ) =
    case msg of
        NoOp ->
            ( state, currentGui )
        Add prop ->
            ( state
            , currentGui
                |> Property.updateAt state.target
                    (Property.append
                        ( "test"
                        , prop
                        )
                    )
            )
        Edit prop -> ( state, currentGui )
        ShowMenu -> ( state, currentGui )


view : Model -> Html Msg
view ( state, tree ) =
    Html.div
        []
        [ preview <| fillTypes <| tree
        , case state.current of
            Just currentProp ->
                editorFor currentProp
            Nothing -> Html.div [] []
        ]


fillTypes : Tron () -> Tron ( ( Path, LabelPath ), Type )
fillTypes =
    Property.reflect
        >> Property.map Tuple.first
        >> Property.map valueToType
        >> Property.addPaths


valueToType : Value -> Type
valueToType _ = Waiting


editorFor : Tron Def -> Html Msg
editorFor _ = Html.div [] []


preview : Tron Def -> Html Msg
preview _ = Html.div [] []

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