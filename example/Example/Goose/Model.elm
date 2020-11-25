module Example.Goose.Model exposing (..)


import Color exposing (Color)



type alias Model =
    { honk : ( Bool, HonkConfig )
    , eye : EyeConfig
    , lookAt : LookDirection
    , shoes : Shoes
    , punk : Bool
    , colors : Colors
    }


init : Model
init =
    { honk =
        ( True
        ,
            { text = "CAI!"
            , size = 36
            , position = ( 0, 0 )
            , color = Color.black
            }
        )
    , eye =
        EyeConfig ( 0, 0 ) 5
    , lookAt = Left
    , shoes = None
    , punk = False
    , colors =
        { feathers = Color.rgb255 236 41 123 -- #ec297b
        , skin = Color.rgb255 253 182 13 -- #fdb60d
        , eye = Color.black
        , background = Color.gray -- Color.white
        , iroquois = Color.black
        }
    }


type alias HonkConfig =
    { text : String
    , size : Int
    , position : ( Float, Float )
    , color : Color
    }


type alias EyeConfig =
    { position : ( Float, Float )
    , size : Float
    }


type LookDirection
    = Left
    | Right


type Shoes
    = None
    | Boots


type alias Colors =
    { feathers : Color
    , skin : Color
    , eye : Color
    , background : Color
    , iroquois : Color
    }
