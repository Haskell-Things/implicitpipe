
module Graphics.Implicit.Viewer.Util where

import Linear
import Data.Map (Map)
import qualified Data.Map

mkScaleTransform :: Float -> M44 Float
mkScaleTransform s =
  (V4 (V4 s 0 0 0)
      (V4 0 s 0 0)
      (V4 0 0 s 0)
      (V4 0 0 0 1)
      )

-- | Get next element in map following `key`
-- or cycle to first.
nextInMap :: Ord a => a -> Map a x -> a
nextInMap key m =
  let cur = Data.Map.findIndex key m
  in  fst
    $ Data.Map.elemAt
        ((cur + 1) `mod` Data.Map.size m)
        m
