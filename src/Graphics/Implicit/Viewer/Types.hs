
module Graphics.Implicit.Viewer.Types
  ( UniformBuffer
  , PrimitiveBuffer
  , ShaderEnvironment(..)
  , ViewerState(..)
  , module Graphics.Implicit.Viewer.Config
  ) where

import Control.Concurrent.MVar (MVar)
import Graphics.GPipe
import Graphics.Implicit.Viewer.Config

type UniformBuffer = (V4 (B4 Float), V3 (B3 Float))
type PrimitiveBuffer = (B3 Float, B3 Float)

data ShaderEnvironment = ShaderEnvironment
  { shaderEnvTriangles     :: PrimitiveArray Triangles PrimitiveBuffer
  , shaderEnvRasterOptions :: (Side, ViewPort, DepthRange)
  }

data ViewerState = ViewerState {
    camYaw           :: Float
  , camPitch         :: Float
  , camZoom          :: Float
  , lastCursorPos    :: (Float, Float)
  , scrollX          :: MVar Float
  , scrollY          :: MVar Float
  , windowWidth      :: Int
  , windowHeight     :: Int
  , lastSample       :: Double
  , fps              :: Double
  , animationRunning :: Bool
  , animationForward :: Bool
  , animationTime    :: Float
  , rotationRunning  :: Bool
  , rotationAngle    :: Float
  , shouldClose      :: Bool
  , conf             :: ViewerConf
  , objectScale      :: Float
  }
