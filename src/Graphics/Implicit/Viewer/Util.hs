
module Graphics.Implicit.Viewer.Util where

import Linear

mkScaleTransform :: Float -> M44 Float
mkScaleTransform s =
  (V4 (V4 s 0 0 0)
      (V4 0 s 0 0)
      (V4 0 0 s 0)
      (V4 0 0 0 1)
      )
