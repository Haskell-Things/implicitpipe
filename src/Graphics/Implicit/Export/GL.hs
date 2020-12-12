module Graphics.Implicit.Export.GL where

import Linear (V4(..))

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.DiscreteAproxable
import Graphics.Implicit.Viewer.Types

-- | Render `SymbolicObj3` to our GPipe pipeline datatype
--
-- objToGL used to return just [(V3 Float, V3 Float)] for positions and normals
-- but grew additional info like barycentric coordinates and color
-- which are needed later in our pipeline
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
    (\(NormedTriangle (a, b, c)) -> zipWith convert [a, b, c] barycentric) ts
  where
    convert ((V3 x y z), (V3 xn yn zn)) bar =
      ( realToFrac <$> V3 x y z
      , VertexInfo {
          viPos         = realToFrac <$> (V3 x y z)
        , viNormal      = (realToFrac <$> V3 xn yn zn)
        , viBarycentric = bar
        , viColor       = V4 1 1 1 1
        }
      )
    barycentric = [ V3 1 0 0, V3 0 1 0, V3 0 0 1 ]
