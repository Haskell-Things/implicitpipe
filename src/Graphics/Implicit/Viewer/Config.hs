
module Graphics.Implicit.Viewer.Config where

import Data.Default

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.DiscreteAproxable
import Graphics.Implicit.Viewer.Demos

-- | Rendering resolution
--
-- @Fixed@ (typically in millimeters) or
-- a function of time:
--
-- @Varied (discreteAprox . (+0.1) . (*5))@
data Resolution =
    Fixed Double
  | Varied (Double -> SymbolicObj3 -> NormedTriangleMesh)

-- | Scale resolution by function
apResolution :: (Double -> Double) -> Resolution -> Resolution
apResolution f (Fixed n) = Fixed (f n)
apResolution f (Varied f') = Varied $ f' . f

meshFunFromResolution
  :: Resolution
  -> Double
  -> SymbolicObj3
  -> NormedTriangleMesh
meshFunFromResolution (Fixed n) = const $ discreteAprox n
meshFunFromResolution (Varied f) = f

data ViewerConf = ViewerConf
  { obj               :: Double
                      -> SymbolicObj3 -- ^ Time parametrized object
  , resolution        :: Resolution   -- ^ Rendering resolution
  , initZoom          :: Float        -- ^ Initial zoom
  , animation         :: Bool         -- ^ Enable animation
  , animationBounce   :: Bool         -- ^ Animation bounces (reverses direction at the limits)
  , animationInitTime :: Float        -- ^ Initial animation time [0..1]
  , animationStep     :: Float        -- ^ Animation time step size
  , rotation          :: Bool         -- ^ Enable rotation
  , rotationStep      :: Float        -- ^ Rotation step size
  , rotationInitAngle :: Float        -- ^ Initial rotation angle (in radians)
  , fudgeBufferSize   :: Int -> Int   -- ^ Temporary buffer size scaling function
  }

instance Default ViewerConf where
  def = ViewerConf
    { obj               = demoLetterI
    , resolution        = Fixed 1
    , initZoom          = 1
    , animation         = False
    , animationBounce   = True
    , animationInitTime = 0
    , animationStep     = 0.001
    , rotation          = False
    , rotationStep      = 0.001
    , rotationInitAngle = 0
    , fudgeBufferSize   = id
    }

-- | Enable animation
animated :: ViewerConf -> ViewerConf
animated c = c {
    animation = True
  -- We scale buffer size by 10 to fit animation frames with
  -- more triangles into our vertex buffer. Until we can resize..
  , fudgeBufferSize = (*10)
  }

-- | Enable preview mode with lower resolution
-- and higher animation and rotation step sizes.
preview :: ViewerConf -> ViewerConf
preview c = c {
    resolution    = apResolution (*2) (resolution c)
  , animationStep = animationStep c * 10
  , rotationStep  = rotationStep c * 10
  }

-- | Enable object rotation
rotating :: ViewerConf -> ViewerConf
rotating c = c { rotation = True }
