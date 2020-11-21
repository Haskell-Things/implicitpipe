module Graphics.Implicit.Viewer.Demos where

import Graphics.Implicit
import Graphics.Implicit.Primitives (getBox)

demoLetterI :: Double -> SymbolicObj3
demoLetterI t =
    scale 2
  $ translate (0, 0, -7)
  $ unionR 1
  [ rect3R 1 (-2,-2,0) (2,2,10)
  , translate (0, 0, 13 - (13 * t)) $ sphere (3 + t)
  ]

demoRotatingAnim :: Double -> SymbolicObj3
demoRotatingAnim t =
    rotate3 (0, 0, t*2*pi)
  $ cylinder2 2 1 2 `ontop` rect3R 0 (-3,-3,-1) (3,3,1)


ontop :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
ontop a b = union [ translate (0, 0, z) a, b ]
  where z =
              let
                  ((_, _, aBottom), _) = getBox a
                  (_, (_ ,_ , bTop)) = getBox b
              in bTop - aBottom



demoTranslatedSymbolic :: SymbolicObj3
demoTranslatedSymbolic = translate (45, 0, 0) demoSymbolic

demoSymbolic :: SymbolicObj3
demoSymbolic = implicit (\(x,y,z) -> x^(4 :: Int) + y^(4 :: Int) + z^(4 :: Int) - 15000) ((-20,-20,-20),(20,20,20))

demoAnimSpheres :: Double -> SymbolicObj3
demoAnimSpheres t = union $ [
   translate (s,0,0) (sphere (5 - s/5))
 , translate (-s,0,0) (sphere (5 - s/5))
 , translate (-5-(s/2), 0, 0) $ rotateExtrude (t * 360) (Just 0) (Left (0, 2)) (Left 0) (translate (4, 0) $ circle 1)
 , translate (-10, 20, 0)
    $ rotate3 (0, t * 2 * pi, 0)
      $ union [
          translate (0, 0, -s) (cylinder2 1 3 7)
        , translate (0, 0, s) $ rotate3 (pi, 0, 0) $  cylinder2 1 3 7 ]
 ] ++
   map (\x -> translate (5 + (fromIntegral x * 2), 5, 0)
    (rect3R 0 (0,0,0) (1, 1, fromIntegral x))
    ) [(0 :: Int)..(round s)]
  where s = (t*7) + 0.01 -- noglitchart

demoScene :: SymbolicObj3
demoScene = union $ [
    rect3R 0 (0,0,0) (20,20,20)
  , translate (20,20,20) (sphere 15)
  , translate (30,20,20) (sphere 5)
  , translate (0,0,25) (rect3R 2 (0,0,0) (10,10,10))
  , translate (25,0,0) (cylinder2 10 1 100)
  , translate (45,0,0) $ difference [
        cylinder 5 (10)
      , cylinder 3 (12)
      ]
  -- This produces all sorts of weird artifacts
  -- and also weird normals when in union
  -- https://github.com/colah/ImplicitCAD/issues/280
  -- , translate (55, 55, 0) demoSymbolic
  ] ++
  map (\x -> translate (0,0,20 + (10 - x) * 11) (rect3R 0 (0,0,0) (10 - x, 10 - x, 10 - x))) [0..9]



