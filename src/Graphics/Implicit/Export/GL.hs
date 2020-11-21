module Graphics.Implicit.Export.GL where

import Linear (V3(..))

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.DiscreteAproxable

objToGL
  :: Double
  -> SymbolicObj3
  -> [(V3 Float, V3 Float)]
objToGL resolution object =
    meshToGL
  . discreteAprox resolution
  $ object

meshToGL
  :: NormedTriangleMesh
  -> [(V3 Float, V3 Float)]
meshToGL (NormedTriangleMesh ts) =
  concatMap
    (\(NormedTriangle (a, b, c)) -> map toV3Pair [a, b, c]) ts
  where
    toV3Pair ((x, y, z), (xn, yn, zn)) =
      ( realToFrac <$> V3 x y z
      , realToFrac <$> V3 xn yn zn)
