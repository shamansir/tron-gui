module Gui.Render.Transform exposing (..)
-- FIXME: do not expose constructors
-- TODO: move conversion to CSS here


type Transform a = Transform Float


type Rotate = Rotate
type Scale = Scale
type MoveX = MoveX

type MoveY = MoveY


rotate : Float -> Transform Rotate
rotate = Transform


scale : Float -> Transform Scale
scale = Transform


moveX : Float -> Transform MoveX
moveX = Transform


moveY : Float -> Transform MoveY
moveY = Transform
