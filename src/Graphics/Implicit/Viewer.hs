{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Implicit.Viewer (
    animate
  , view
  , viewer
  , ViewerConf(..)
  , animateMain
  , viewerMain
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Default

import Graphics.GPipe hiding ((^-^), rotate)
import Graphics.UI.GLFW (WindowHint(..))
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Implicit
import Graphics.Implicit.Primitives (getBox)
import Graphics.Implicit.Export.GL
import Graphics.Implicit.Viewer.Types
import Graphics.Implicit.Viewer.Util

-- | View `SymbolicObj3` object using OpenGL viewer
view :: SymbolicObj3 -> IO ()
view o = viewer (def { obj = const o } )

-- | Animate `SymbolicObj3` object using OpenGL viewer
--
-- Object can be parameterized by animation time `Double`
-- in the interval [0..1].
animate :: (Double -> SymbolicObj3) -> IO ()
animate a = viewer $ animated (def { obj = a } )

viewerMain :: IO ()
viewerMain = viewer $ rotating def

animateMain :: IO ()
animateMain = viewer $ preview $ animated $ (def { initZoom = 2 })

-- | Configurable viewer
--
-- Same viewer is used for both static viewer
-- and animation.
--
viewer :: ViewerConf -> IO ()
viewer config@ViewerConf{..} = do
  let
      (ba, bb) = getBox (obj 0)
      rmax (x, y, z) = maximum [x, y, z]

      -- ratio perserving scaling
      s = realToFrac $ rmax (bb - ba)

  scrollX' <- newMVar 0
  scrollY' <- newMVar 0

  runContextT GLFW.defaultHandleConfig $ do
    triangles :: Buffer os PrimitiveBuffer
      <- newBuffer
          $ fudgeBufferSize
          $ length
          $ meshToGL
          $ meshFunFromResolution resolution 1 (obj 0)

    writeBuffer triangles 0
      $ meshToGL
      $ meshFunFromResolution resolution 1 (obj 0)

    win <- newWindow
      (WindowFormatColorDepth SRGB8 Depth16)
      ((GLFW.defaultWindowConfig "GPipe ImplicitCAD viewer")
        { GLFW.configHints = [ WindowHint'Samples $ Just 16 ] }
      )

    -- Try updating scrolling vars
    void . GLFW.setScrollCallback win . pure $
        \dx dy -> do
          void $ tryPutMVar scrollX' (realToFrac dx)
          void $ tryPutMVar scrollY' (realToFrac dy)

    -- Create a buffer for the uniform values
    uniform :: Buffer os (Uniform UniformBuffer) <- newBuffer 1

    -- Create the shader
    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream shaderEnvTriangles

      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let
          proj (V3 px py pz, normal) =
            ( modelViewProj !* V4 px py pz 1
            , fmap Flat $ normMat !* normal)
          projected = proj <$> primitiveStream

      fragmentStream <- rasterize
        shaderEnvRasterOptions
        projected

      let
          getZ (V4 _ _ z _) = z

          light normal =
            0.1 -- global illumination
            + (
                -- red light from front right
                (V3 0.8 0   0   ^* (clamp (normal `dot` V3 1 (-1) 1) 0 1))
                -- green from front left
              + (V3 0   0.8 0   ^* (clamp (normal `dot` V3 (-1) (-1) 1) 0 1))
                -- blue from bottom
              + (V3 0   0   0.8 ^* (clamp (normal `dot` V3 0 0 (-1)) 0 1))
              )

          litFrags = light <$> fragmentStream
          litFragsWithDepth = withRasterizedInfo
              (\a x -> (a, getZ $ rasterizedFragCoord x)) litFrags
          colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawWindowColorDepth
        (const (win, colorOption, depthOption))
        litFragsWithDepth

      return ()

    -- Run the loop
    loop win shader triangles uniform
      ViewerState {
        camYaw           = 0
      , camPitch         = 0
      , camZoom          = initZoom
      , lastCursorPos    = (0, 0)
      , scrollX          = scrollX'
      , scrollY          = scrollY'
      , lastSample       = 0
      , fps              = 0
      , animationRunning = animation
      , animationForward = True
      , animationTime    = animationInitTime
      , rotationRunning  = rotation
      , rotationAngle    = rotationInitAngle
      , shouldClose      = False
      , conf             = config
      , objectScale      = (1/s)
      , windowWidth      = 0
      , windowHeight     = 0
      }

loop
  :: forall os .  Window os RGBFloat Depth
  -> (ShaderEnvironment -> Render os ())
  -> Buffer os PrimitiveBuffer
  -> Buffer os (Uniform UniformBuffer)
  -> ViewerState
  -> ContextT GLFW.Handle os IO ()
loop win shader triangles uniform viewerState = do

  newViewerState@ViewerState{..} <- updateViewerState win viewerState
  let ViewerConf{..} = conf

  let modelRot = fromQuaternion (axisAngle (V3 0 0 1) (-rotationAngle))

      modelMat =
            mkTransformationMat modelRot (pure 0)
        !*! mkScaleTransform objectScale

      projMat = perspective (pi/2) (fromIntegral windowWidth / fromIntegral windowHeight) 0.1 100

      eye = (V3 0 (-1) 1)
      lookAtPoint = (V3 0 0 0)

      cameraMatrix :: M44 Float
      cameraMatrix =
        lookAt
          eye
          lookAtPoint
          (V3 0 0 1) -- up vector
        !*!
        mkTransformation
          (axisAngle (V3 1 0 0) (camPitch))
          (pure 0)
        !*!
        mkTransformation
          (axisAngle (V3 0 0 1) (camYaw))
          (pure 0)
        !*!
        mkScaleTransform camZoom

      viewMat = cameraMatrix
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot

  -- Write this frames uniform value
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  when animation $ do
    -- clean buffer
    writeBuffer triangles 0
      $ take (bufferLength triangles)
      $ repeat 0
    -- update with new mesh
    writeBuffer triangles 0
      $ meshToGL
      $ meshFunFromResolution
          resolution
          (realToFrac animationTime)
      $ obj
          (realToFrac animationTime)

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane

    vertexArray <- newVertexArray triangles
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader
      $ ShaderEnvironment
          primitiveArray
          ( FrontAndBack
          , ViewPort 0 (V2 windowWidth windowHeight)
          , DepthRange 0 1
          )
  swapWindowBuffers win

  unless shouldClose
    $ loop win shader triangles uniform newViewerState

updateViewerState
  :: forall os .  Window os RGBFloat Depth
  -> ViewerState
  -> ContextT GLFW.Handle os IO ViewerState
updateViewerState win oldViewerState@ViewerState{..} = do
  let ViewerConf{..} = conf

  (Just now) <- liftIO $ GLFW.getTime

  mCursor <- GLFW.getCursorPos win

  let (cursorX, cursorY) = case mCursor of
        Just r -> r
        Nothing -> (0, 0)

      (oldCursorX, oldCursorY) = lastCursorPos

      (cursorDeltaX, cursorDeltaY) =
        ( realToFrac cursorX - oldCursorX
        , realToFrac cursorY - oldCursorY)

      dt = now - lastSample


  qKey <- GLFW.getKey win GLFW.Key'Q
  winShouldClose <- GLFW.windowShouldClose win
  let shouldClose' =
           (winShouldClose == Just True)
        || (qKey == Just GLFW.KeyState'Pressed)

  (V2 w h) <- getFrameBufferSize win

  mouseButton1 <- GLFW.getMouseButton win GLFW.MouseButton'1
  let (dYaw, dPitch) = case mouseButton1 of
        Just GLFW.MouseButtonState'Pressed
          -> (cursorDeltaX / 100, cursorDeltaY / 100)
        _ -> (0, 0)

  spaceKey <- GLFW.getKey win GLFW.Key'Space
  let faster = case spaceKey of
        Just GLFW.KeyState'Pressed -> (*10)
        _ -> id

  mdY <- liftIO $ tryTakeMVar scrollY
  let newZoom = case mdY of
                  Nothing -> camZoom
                  Just dy -> camZoom + (dy/10)

      animDirection = if animationForward then 1
                                          else -1
      animTime =
        if animation then animationTime + (faster $ animDirection * animationStep)
                     else animationTime

      nextOutOfBounds =
               animationForward && animationTime + animationStep > 1
        || not animationForward && animationTime - animationStep < 0

      rotAngle =
        if rotation then (rotationAngle + faster rotationStep) `mod''` (2*pi)
                    else rotationAngle

  return $ oldViewerState {
      camPitch         = (dPitch + camPitch) `mod''` (2*pi)
    , camYaw           = (dYaw + camYaw) `mod''` (2*pi)
    , camZoom          = newZoom
    , lastCursorPos    = (realToFrac cursorX, realToFrac cursorY)
    , windowWidth      = w
    , windowHeight     = h
    , lastSample       = now
    , fps              = 1 / dt
    , animationRunning = not nextOutOfBounds || (nextOutOfBounds && animationBounce)
    , animationTime    = animTime
    , animationForward = (if nextOutOfBounds then not else id) animationForward
    , rotationAngle    = rotAngle
    , shouldClose      = shouldClose'
    }
