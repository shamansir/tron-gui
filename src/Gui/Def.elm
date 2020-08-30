module Gui.Def exposing (..)


type alias Label = String

type Icon = Icon String

type alias ItemChosen = ( Int, Label )

type NestPos = NestPos (List Int) -- just path by indices


type Control setup value msg =
    Control setup value (value -> msg)


type alias Shape = ( Int, Int )


type alias Axis =
    { min : Float
    , max : Float
    , step : Float
    , roundBy : Int
    , default : Float
    }


type alias Nest umsg =
    { focus: Int
    , shape: Shape
    , cells: Cells umsg
    }


type ExpandState
    = Expanded
    | Collapsed


-- type ExpandDirection
--     = Up
--     | Down


type ToggleState
    = TurnedOn
    | TurnedOff


type SelectionState
    = Selected
    | NotSelected


-- type AlterKnob
--     = Up
--     | Down
--     | Stay


type AlterKnob
    = Stay
    | Alter Float -- from -0.5 to 0.5


type AlterXY
    = Stay_
    | Alter_ ( Float, Float ) -- both from -0.5 to 0.5


type Over msg
    = Anything
    | Number (Control Axis Float msg)
    | Coordinate (Control ( Axis, Axis ) ( Float, Float ) msg)
    | Text (Control () String msg)
    | Toggle (Control () ToggleState msg)
    | Action (Control (Maybe Icon) () msg)
    | Nested
        (Control
            ( Shape, List ( Label, Over msg ) )
            ( ( Int, Label ), Over msg )
            msg
        )


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
