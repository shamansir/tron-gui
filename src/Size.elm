module Size exposing (..)


type Size a = Size ( Int, Int )


type SizeF a = SizeF (Float, Float)


type Pixels = Pixels


type Cells = Cells


size ( w, h ) = Size ( w, h )


sizeF ( w, h ) = SizeF ( w, h )
