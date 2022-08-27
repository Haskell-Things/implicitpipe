{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Implicit.Viewer.Demos where

import Linear
import Graphics.Implicit
import Graphics.Implicit.Primitives

demoLetterI :: Double -> SymbolicObj3
demoLetterI t =
    scale 2
  $ translate (V3 0 0 (-7))
  $ unionR 1
  [ translate (V3 0 0 5) $ withRounding 1 $ cube True (V3 4 4 10)
  , translate (V3 0 0 (13 - 13 * t)) $ sphere (3 + t)
  ]

demoRotatingAnim :: Double -> SymbolicObj3
demoRotatingAnim t =
    rotate3 (V3 0 0 (t*2*pi))
  $ cylinder2 2 1 2 `ontop` cube True (V3 6 6 2)

ontop :: SymbolicObj3 -> SymbolicObj3 -> SymbolicObj3
ontop a b = union [ translate (V3 0 0 z) a, b ]
  where z = let
                ((V3 _ _ aBottom), _) = getBox a
                (_, (V3 _ _ bTop)) = getBox b
            in bTop - aBottom

demoTranslatedSymbolic :: SymbolicObj3
demoTranslatedSymbolic = translate (V3 45 0 0) demoSymbolic

demoSymbolic :: SymbolicObj3
demoSymbolic = implicit
  (\(V3 x y z) -> x^(4 :: Int) + y^(4 :: Int) + z^(4 :: Int) - 15000)
  (pure (-20), pure 20)

demoI :: SymbolicObj3
demoI = implicit
  (\(V3 x y z) -> 0.9*(x^2+y^2 + z^6/2 -1) + 0.1* ((x/4)^2+y^2 + z^2) - 1)
  (pure (-1), pure 1)

demoAnimSpheres :: Double -> SymbolicObj3
demoAnimSpheres t = union $ [
   translate (V3 s 0 0) (sphere (5 - s/5))
 , translate (V3 (-s) 0 0) (sphere (5 - s/5))
 , translate (V3 (-5-(s/2)) 0 0)
    $ rotateExtrude (t * 360) (Left (V2 0 2)) (Left 0)
      $ translate (V2 4 0) $ circle 1
 , translate (V3 (-10) 20 0)
    $ rotate3 (V3 0 (t * 2 * pi) 0)
      $ union [
          translate (V3 0 0 (-s)) (cylinder2 1 3 7)
        , translate (V3 0 0 s) $ rotate3 (V3 pi 0 0) $  cylinder2 1 3 7 ]
 ] ++
   map (\x -> translate (V3 (5 + (fromIntegral x * 2)) 5 0)
    (cube False (V3 1 1 (fromIntegral x)))
    ) [(0 :: Int)..(round s)]
  where s = (t*7) + 0.01 -- noglitchart

demoScene :: SymbolicObj3
demoScene = union $ [
    cube False (pure 20)
  , translate (pure 20) (sphere 15)
  , translate (V3 30 20 20) (sphere 5)
  , translate (V3 0 0 25) (withRounding 2 $ cube False (pure 10))
  , translate (V3 25 0 0) (cylinder2 10 1 100)
--  , translate (V3 45 0 0) $ difference
--      ( cylinder 5 (10) )
--      [ cylinder 3 (12) ]
  -- This produces all sorts of weird artifacts
  -- and also weird normals when in union
  -- https://github.com/colah/ImplicitCAD/issues/280
  -- , translate (V3 55 55 0) demoSymbolic
  ] ++
  map (\x -> translate (V3 0 0 (20 + (10 - x) * 11)) (cube False (pure $ 10 - x))) [0..9]

circ :: SymbolicObj2
circ = implicit (\(V2 x y) -> x^(2 :: Int) + y^(2 :: Int) - 1)  (pure (-10), (pure 10))

sph :: SymbolicObj2
sph = implicit (\(V2 x y) -> 10*x^(2 :: Int) + y^(2 :: Int) - 1) (pure (-10), (pure 10))

-- | Mix two objects
--
-- Works for intersecting objects mostly
--
-- @mixed 0 objA objB@ = objA@
-- @mixed 1 objA objB@ = objB@
-- @mixed 0.5 objA objB@ = "something in-between of objA and objB shapes" @
--
-- Example:
-- @
--   cubeOSphere s n = mixed s
--    (cube True (pure n))
--    (sphere (sqrt (3 * (n/2) ^2) ))
-- @
mixed
  :: (Object obj f Double, Semigroup obj)
  => ℝ
  -> obj
  -> obj
  -> obj
mixed s a b = implicit
  (\i ->
    (1 - s) * getImplicit a i
  +      s  * getImplicit b i
  )
  (getBox (a <> b))
