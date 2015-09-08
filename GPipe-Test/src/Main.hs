{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
module Main where

import Control.Applicative
import Control.Monad
import "transformers" Control.Monad.IO.Class

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import qualified "JuicyPixels" Codec.Picture as Juicy
import qualified "JuicyPixels" Codec.Picture.Types as Juicy
import "linear" Linear

main =
  runContextT GLFW.newContext (ContextFormatColorDepth SRGB8 Depth16) $ do
    -- Create vertex data buffers
    positions :: Buffer os (B2 Float) <- newBuffer 4
    normals   :: Buffer os (B3 Float) <- newBuffer 6
    tangents  :: Buffer os (B3 Float) <- newBuffer 6
    writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
    writeBuffer normals 0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
    writeBuffer tangents 0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]

    -- Make a Render action that returns a PrimitiveArray for the cube
    let makePrimitives = do
          pArr <- newVertexArray positions
          nArr <- newVertexArray normals
          tArr <- newVertexArray tangents
          let sideInstances = zipVertices (,) nArr tArr
          return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances

    -- Load image into texture
    Right (Juicy.ImageYCbCr8 image) <- liftIO $ Juicy.readImage "image.jpg"
    let size = V2 (Juicy.imageWidth image) (Juicy.imageHeight (image))
    tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
    writeTexture2D tex 0 0 size $ Juicy.pixelFold getJuicyPixel [] image
    generateTexture2DMipmap tex

    -- Create a buffer for the uniform values
    uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

    -- Create the shader
    shader <- compileShader $ do
      sides <- fmap makeSide <$> toPrimitiveStream primitives
      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let filterMode = SamplerFilter Linear Linear Linear (Just 4)
          edgeMode = (pure ClampToEdge, undefined)
          projectedSides = proj modelViewProj normMat <$> sides
      samp <- newSampler2D (const (tex, filterMode, edgeMode))

      fragNormalsUV <- rasterize rasterOptions projectedSides
      let litFrags = light samp <$> fragNormalsUV
          litFragsWithDepth = withRasterizedInfo
              (\a x -> (a, getZ $ rasterizedFragCoord x)) litFrags
          colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth

    -- Run the loop
    loop shader makePrimitives uniform 0

loop shader makePrimitives uniform angle = do
  -- Write this frames uniform value
  size@(V2 w h) <- getContextBuffersSize
  let modelRot = fromQuaternion (axisAngle (V3 1 0.5 0.3) angle)
      modelMat = mkTransformationMat modelRot (pure 0)
      projMat = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
      viewMat = mkTransformationMat identity (- V3 0 0 5)
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearContextColor 0 -- Black
    clearContextDepth 1 -- Far plane
    prims <- makePrimitives
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapContextBuffers

  closeRequested <- GLFW.windowShouldClose
  unless closeRequested $
    loop shader makePrimitives uniform ((angle + 0.005) `mod''` (2*pi))

getJuicyPixel xs _x _y pix =
  let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs

getZ (V4 _ _ z _) = z -- Some day I'll learn to use lenses instead...

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (B2 Float, (B3 Float, B3 Float))
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

-- Project the sides coordinates using the instance's normal and tangent
makeSide (p@(V2 x y), (normal, tangent)) =
  (V3 x y 1 *! V3 tangent bitangent normal, normal, uv)
  where bitangent = cross normal tangent
        uv = (p + 1) / 2

-- Project the cube's positions and normals with ModelViewProjection matrix
proj modelViewProj normMat (V3 px py pz, normal, uv) =
  (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))

-- Set color from sampler and apply directional light
light samp (normal, uv) =
  sample2D samp SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1)

-- eof
