{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.Implicit.Viewer.Shaders where

import Data.Default
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map
import Graphics.GPipe
import Graphics.Implicit.Viewer.Types

type ShaderVertexInfo = VertexInfo (V3 FFloat) (V3 FFloat) (V3 FFloat) (V4 FFloat)
type FragStream = FragmentStream ShaderVertexInfo

allShaders
  :: Map Int ((String, Window os RGBAFloat Depth
                        -> FragStream
                        -> Shader os (ShaderEnvironment os) ()))
allShaders = Data.Map.fromList $ zip [0..]
  [ s "default"    (lightShaded 0.1)
  , s "wireframe"  alphaWireframe
  , s "edges"      edges
  , s "edgy"       edgy
  , s "pointy"     pointy
  , s "nospecular" (lightShaded 0)
  ]
  where s = (,)

asumShaderByID
  :: forall os
   . ((ShaderEnvironment os) -> Int)
  -> Window os RGBAFloat Depth
  -> FragStream
  -> Shader os (ShaderEnvironment os) ()
asumShaderByID getIDFromState win fragStream =
    asum
  $ Data.Map.mapWithKey guardByName allShaders
  where
    guardByName sid (_name, shader) = do
      guard' ((==sid) . getIDFromState)
      shader win fragStream

computeLight :: FFloat -> V3 FFloat -> ShaderVertexInfo -> V4 FFloat
computeLight specularIntensity eye VertexInfo{..} =

  let normal = viNormal
      dirR = signorm $ V3    1 (-1)    1 - viPos
      dirG = signorm $ V3 (-1) (-1)    1 - viPos
      dirB = signorm $ V3    0    0 (-1) - viPos

      -- simplified Blinn-Phong
      -- lightDir = lightPos - viPos -- this is the same as viewDir for us
      -- .. and ..
      -- halfVector = signorm $ lightDir + viewDir
      -- .. becomes ..
      -- halfVector = signorm $ 2 * viewDir = signorm viewDir
      viewDir = eye - viPos
      halfVector = signorm viewDir
      specular = maxB (viNormal `dot` halfVector) 0
  in
   specularIntensity *^ (V4 1 1 1 1) ^* (specular ** 32)
     + (1 *^ opaque $
        0.1 -- global illumination
      + (
          -- red light from front right
          (V3 0.8 0   0   ^* (maxB (normal `dot` dirR) 0))
          -- green from front left
        + (V3 0   0.8 0   ^* (maxB (normal `dot` dirG) 0))
          -- blue from bottom
        + (V3 0   0   0.8 ^* (maxB (normal `dot` dirB) 0))
        ))

lightShaded
  :: forall os . FFloat
  -> Window os RGBAFloat Depth
  -> FragStream
  -> Shader os (ShaderEnvironment os) ()
lightShaded i win fragStream = do
  eye <- getUni bEye
  let
    litFrags = (computeLight i eye) <$> fragStream

  drawWindowColorDepth
    (const (win, def, def))
    (withDepth litFrags)

alphaWireframe
  :: forall os s . Window os RGBAFloat Depth
  -> FragStream
  -> Shader os s ()
alphaWireframe win fragStream = do
  let
    fragShader bar =
      let
        w = smoothstep 0 <$> ((1.5*) . fwidth <$> bar) <*> bar
        (V3 i j k) = w
        edgeFactor = minB (minB i j) k
      in
        (V4 1 0 0 ((1.0 - edgeFactor) * 0.95))

  drawWindowColor
     (const (win, blendAlpha))
   $ fragShader . viBarycentric <$> fragStream

edgy
  :: forall os . Window os RGBAFloat Depth
  -> FragStream
  -> Shader os (ShaderEnvironment os) ()
edgy win fragStream = do
  let
    fragShader VertexInfo{..} =
      let
        eps = 0.01
        (V3 i j k) = viBarycentric
      in
        ifThenElse' (i <=* eps ||* j <=* eps ||* k <=* eps)
          (opaque (V3 0 0 0))
          (V4 1 1 1 0.5)

    wireFrags = fragShader <$> fragStream

  drawWindowColor
   (const (win, blendAlpha))
   (wireFrags)

edges
  :: forall os . Window os RGBAFloat Depth
  -> FragStream
  -> Shader os (ShaderEnvironment os) ()
edges win fragStream = do
  eye <- getUni bEye
  let
    fragShader vi@VertexInfo{..} =
      let
        eps = 0.02
        (V3 i j k) = viBarycentric
      in
        ifThenElse' (i <=* eps ||* j <=* eps ||* k <=* eps)
          (opaque $ V3 0 0 0)
          (computeLight 0.1 eye vi)

    litFrags = fragShader <$> fragStream

  drawWindowColorDepth
    (const (win, def, def))
    (withDepth litFrags)

pointy
  :: forall os . Window os RGBAFloat Depth
  -> FragStream
  -> Shader os (ShaderEnvironment os)()
pointy win fragStream = do
  eye <- getUni bEye
  let
    fragShader vi@VertexInfo{..} =
      let eps = 0.02
          (V3 i j k) = viBarycentric
      in
      ifThenElse' (i >=* 1 - eps ||* j >=* 1 - eps ||* k >=* 1 - eps)
        (V4 0 0 0 1)
        (computeLight 0.1 eye vi)

    litFrags = fragShader <$> fragStream

  drawWindowColorDepth
    (const (win, def, def))
    (withDepth litFrags)

blendAlpha :: ContextColorOption RGBAFloat
blendAlpha = ContextColorOption
      (BlendRgbAlpha
        (FuncAdd, FuncAdd)
        ( BlendingFactors SrcAlpha OneMinusSrcAlpha
        , BlendingFactors SrcAlpha OneMinusSrcAlpha)
        (V4 0.5 0 0 0))
      (pure True)

instance Default (ContextColorOption RGBAFloat) where
  def = ContextColorOption NoBlending (pure True)

instance Default DepthOption where
  def = DepthOption Less True

withDepth :: FragmentStream a -> FragmentStream (a, FFloat)
withDepth frags = withRasterizedInfo
    (\a x -> (a, getZ $ rasterizedFragCoord x)) frags
  where getZ (V4 _ _ z _) = z

transparent :: (Num a) => V3 a -> V4 a
transparent = vector

opaque :: (Num a) => V3 a -> V4 a
opaque = point
