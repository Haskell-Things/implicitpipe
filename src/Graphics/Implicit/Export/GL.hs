module Graphics.Implicit.Export.GL where

import Linear (V3(..), V4(..))

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.DiscreteAproxable
import Graphics.Implicit.Viewer.Types

type NativeVertexInfo = VertexInfo (V3 Float) (V3 Float) (V3 Float) (V4 Float)

objToGL
  :: Double
  -> SymbolicObj3
  -> [(V3 Float, NativeVertexInfo)]
objToGL resolution' object' =
    meshToGL
  . discreteAprox resolution'
  $ object'

meshToGL
  :: NormedTriangleMesh
  -> [(V3 Float, NativeVertexInfo)]
meshToGL (NormedTriangleMesh ts) =
  concatMap
    (\(NormedTriangle (a, b, c)) -> zipWith toV3Pair [a, b, c] barycentric) ts
  where
    toV3Pair ((x, y, z), (xn, yn, zn)) bar =
      ( realToFrac <$> V3 x y z
      , VertexInfo {
          viPos         = realToFrac <$> (V3 x y z)
        , viNormal      = (realToFrac <$> V3 xn yn zn)
        , viBarycentric = bar
        , viColor       = V4 1 1 1 1
        }
      )
    barycentric = [ V3 1 0 0, V3 0 1 0, V3 0 0 1 ]
