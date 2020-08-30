module Gui.Control exposing (..)


type Control setup value msg =
    Control setup value (value -> msg)


type alias Label = String

type Icon = Icon String

type alias Shape = ( Int, Int )


type alias Axis =
    { min : Float
    , max : Float
    , step : Float
    , roundBy : Int
    , default : Float
    }


type ExpandState
    = Expanded
    | Collapsed


type ToggleState
    = TurnedOn
    | TurnedOff


type AlterKnob
    = Stay
    | Alter Float -- from -0.5 to 0.5


type AlterXY
    = Stay_
    | Alter_ ( Float, Float ) -- both from -0.5 to 0.5


type Path = Path (List Int)


type alias GroupControl msg =
    Control
        { shape : Shape
        , items : List ( Label, Over msg )
        }
        { expanded : ExpandState
        , focus :
            Maybe
                { index : Int
                , position : ( Int, Int )
                , label : Label
                , at : Over msg
                }
        }
        msg


type Over msg
    = Anything
    | Number (Control Axis Float msg)
    | Coordinate (Control ( Axis, Axis ) ( Float, Float ) msg)
    | Text (Control () String msg)
    | Toggle (Control () ToggleState msg)
    | Action (Control (Maybe Icon) () msg)
    | Group (GroupControl msg)


cellWidth = 70
cellHeight = 70
cellMargin = 5


knobDistance = cellHeight * 4


labelColor = "white"
baseColor = "aqua"
onColor = "green"
offColor = "red"
nothingColor = "darkgray"
lineWidth = "2"
