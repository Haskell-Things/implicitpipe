{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
module Main where

import Control.Monad
import Text.Printf (printf)
import Control.Monad.IO.Class
import Data.Proxy

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import qualified "JuicyPixels" Codec.Picture as Juicy
import qualified "JuicyPixels" Codec.Picture.Types as Juicy

main =
  runContextT (Proxy :: Proxy GLFW.Handle) GLFW.defaultHandleConfig $ do
    -- Create vertex data buffers
    positions :: Buffer os (B2 Float) <- newBuffer 4
    normals   :: Buffer os (B3 Float) <- newBuffer 6
    tangents  :: Buffer os (B3 Float) <- newBuffer 6
    writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
    writeBuffer normals 0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
    writeBuffer tangents 0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]

    win <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "test it!")

    -- Spew scroll info
    GLFW.setScrollCallback win . pure $
        \dx dy -> printf "scroll dx%v dy%v on %v\n" dx dy

    -- Make a Render action that returns a PrimitiveArray for the cube
    let makePrimitives = do
          pArr <- newVertexArray positions
          nArr <- newVertexArray normals
          tArr <- newVertexArray tangents
          let sideInstances = zipVertices (,) nArr tArr
          return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances

    -- Load image into texture
    im <- liftIO $ Juicy.readImage "image.jpg"

    let image = case im of
            Right (Juicy.ImageYCbCr8 i) -> i
            Right _ -> error "Got unexpected image color space"
            Left s -> error s
        size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)

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

      drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

    -- Run the loop
    loop win shader makePrimitives uniform 0

loop win shader makePrimitives uniform angleRot = do
  Just (cursorX, cursorY) <- GLFW.getCursorPos win
  mouseButton1 <- GLFW.getMouseButton win GLFW.MouseButton'1
  spaceKey <- GLFW.getKey win GLFW.Key'Space
  shouldClose <- GLFW.windowShouldClose win
--liftIO $ printf "cursorPos x%v y%v, mouseButton1 %v, spaceKey %v, shouldClose %v\n"
--  cursorX cursorY (show mouseButton1) (show spaceKey) (show shouldClose)
  -- Write this frames uniform value
  size@(V2 w h) <- getWindowSize win
  let modelRot = fromQuaternion (axisAngle (V3 1 0.5 0.3) angleRot)
      modelMat = mkTransformationMat modelRot (pure 0)
      projMat = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
      viewMat = mkTransformationMat identity (- V3 0 0 5)
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane
    prims <- makePrimitives
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapWindowBuffers win

  withContextWindow win $ mapM_ (flip GLFW.mainstep GLFW.Poll)

  Just closeRequested <- GLFW.windowShouldClose win
  unless closeRequested $
    loop win shader makePrimitives uniform ((angleRot + 0.005) `mod''` (2*pi))

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
