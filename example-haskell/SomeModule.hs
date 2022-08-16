{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SomeModule where

import Control.Applicative
import Linear
import Graphics.Implicit
import Graphics.Implicit.Primitives
--import Graphics.Implicit.ObjectUtil.GetBoxShared

import SomeOtherModule

--res = 0.016
res = 0.5

ilerp
  :: forall obj f . (Object obj (f ℝ), Applicative f, Semigroup obj)
  => ℝ
  -> obj
  -> obj
  -> obj
ilerp s a b = implicit
  (\i ->
    (1 - s) * getImplicit a i
  +      s  * getImplicit b i
  )
  $ --(getBox a) <> (getBox b)
  getBox $
    (scale :: f ℝ -> obj -> obj) (pure (1-s)) a
    <> (scale :: f ℝ -> obj -> obj) (pure s) b
--        <> b

ilerp'
  :: forall obj f . (Object obj (f ℝ), Applicative f, Semigroup obj)
  => ℝ
  -> obj
  -> obj
  -> obj
ilerp' s a b = implicit
  (\i ->
    (1 - s) * getImplicit a i
  +      s  * getImplicit b i
  )
  $ --(getBox a) <> (getBox b)
    let (a1 :: f ℝ, b1) = getBox a
        (a2 :: f ℝ, b2) = getBox a
        sa1 = liftA2 (*) (pure $ 1 - s) a1
        sb1 = liftA2 (*) (pure $ 1 - s) b1
        sa2 = liftA2 (*) (pure s) a2
        sb2 = liftA2 (*) (pure s) b2
    in pointsBox [sa1, sb1, sa2, sb2]

pointsBox :: (Applicative f, Ord a, Num a) => [f a] -> (f a, f a)
pointsBox [] = (pure 0, pure 0)
pointsBox (a : as) = (foldr (liftA2 min) a as, foldr (liftA2 max) a as)

obj = ilerp' 1.0 (cube False (pure 3)) (cylinder2 7 5 6)
--obj = cube False (V3 3 0.095 0.045)

objz = union $ [
    cube False (V3 20 20 14)
  , translate (V3 20 20 20) (sphere 15)
  , translate (V3 30 20 20) (sphere 5)
  , translate (V3 0 0 25) (withRounding 2 $ cube False (pure 10))
  , translate (V3 25 0 0) (cylinder2 10 4 10)
  , translate (V3 (-25) 0 0) obj'
  ]

main :: IO ()
main = writeSTL res "example.stl" obj
