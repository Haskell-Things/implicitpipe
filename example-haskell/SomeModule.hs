module SomeModule where

import Linear
import Graphics.Implicit
import Graphics.Implicit.Primitives

import SomeOtherModule

res = 2

obj = union $ [
    cube False (V3 20 20 14)
  , translate (V3 20 20 20) (sphere 15)
  , translate (V3 30 20 20) (sphere 5)
  , translate (V3 0 0 25) (withRounding 2 $ cube False (pure 10))
  , translate (V3 25 0 0) (cylinder2 10 4 10)
  , translate (V3 (-25) 0 0) obj'
  ]

main :: IO ()
main = writeSTL res "example.stl" obj
