{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- due to getUni which explodes when given signature
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Graphics.Implicit.Viewer.Types
  ( PrimitiveBuffer
  , VertexInfo(..)
  , ShaderEnvironment(..)
  , Uniforms(..)
  , ViewerState(..)
  , getUni
  , module Graphics.Implicit.Viewer.Config
  ) where


import Control.Arrow
import Control.Concurrent.MVar (MVar)
import Graphics.GPipe
import Graphics.Implicit.Viewer.Config

type PrimitiveBuffer =
  ( B3 Float
  , VertexInfo
      (B3 Float)
      (B3 Float)
      (B3 Float)
      (B4 Float)
  )

-- Additional information for vertex
-- * world position for lighting
-- * its normal vector
-- * barycentric coordinates for wireframe shading
-- * object color with alpha
data VertexInfo posType normalType barType colorType = VertexInfo
  { viPos         :: posType
  , viNormal      :: normalType
  , viBarycentric :: barType
  , viColor       :: colorType
  }

data Uniforms os = Uniforms
  { bMvpMat   :: Buffer os (Uniform (V4 (B4 Float)))
  , bModelMat :: Buffer os (Uniform (V4 (B4 Float)))
  , bNormMat  :: Buffer os (Uniform (V3 (B3 Float)))
  , bEye      :: Buffer os (Uniform (B3 Float))
  }

getUni which = getUniform (\state -> (which $ shaderEnvUniforms state, 0))

data ShaderEnvironment os = ShaderEnvironment
  { shaderEnvTriangles     :: PrimitiveArray Triangles PrimitiveBuffer
  , shaderEnvRasterOptions :: (Side, ViewPort, DepthRange)
  , shaderEnvFragName      :: String
  , shaderEnvUniforms      :: Uniforms os
  , shaderEnvFlatNormals   :: Bool
  }

data ViewerState = ViewerState {
    camYaw            :: Float
  , camPitch          :: Float
  , camZoom           :: Float
  , lastCursorPos     :: (Float, Float)
  , scrollX           :: MVar Float
  , scrollY           :: MVar Float
  , windowWidth       :: Int
  , windowHeight      :: Int
  , lastSample        :: Double
  , fps               :: Double
  , animationRunning  :: Bool
  , animationForward  :: Bool
  , animationTime     :: Float
  , rotationRunning   :: Bool
  , rotationAngle     :: Float
  , shouldClose       :: Bool
  , conf              :: ViewerConf
  , objectScale       :: Float
  , shaderName        :: String
  , shaderFlatNormals :: Bool
  }

-- Instances for `VertexInfo` so it can be used
-- in shaders.
instance (BufferFormat a, BufferFormat b, BufferFormat c, BufferFormat d) => BufferFormat (VertexInfo a b c d) where
    type HostFormat (VertexInfo a b c d) = VertexInfo (HostFormat a) (HostFormat b) (HostFormat c) (HostFormat d)


    toBuffer = proc ~(VertexInfo a b c d) -> do
                 a' <- toBuffer -< a
                 b' <- toBuffer -< b
                 c' <- toBuffer -< c
                 d' <- toBuffer -< d
                 returnA -< VertexInfo a' b' c' d'

instance (VertexInput a, VertexInput b, VertexInput c, VertexInput d) => VertexInput (VertexInfo a b c d) where
    type VertexFormat (VertexInfo a b c d) = VertexInfo (VertexFormat a) (VertexFormat b) (VertexFormat c) (VertexFormat d)


    toVertex = proc ~(VertexInfo a b c d) -> do
                 a' <- toVertex -< a
                 b' <- toVertex -< b
                 c' <- toVertex -< c
                 d' <- toVertex -< d
                 returnA -< VertexInfo a' b' c' d'

instance (FragmentInput a, FragmentInput b, FragmentInput c, FragmentInput d) => FragmentInput (VertexInfo a b c d) where
    type FragmentFormat (VertexInfo a b c d) = VertexInfo (FragmentFormat a) (FragmentFormat b) (FragmentFormat c) (FragmentFormat d)

    toFragment = proc ~(VertexInfo a b c d) -> do
                   a' <- toFragment -< a
                   b' <- toFragment -< b
                   c' <- toFragment -< c
                   d' <- toFragment -< d
                   returnA -< VertexInfo a' b' c' d'
