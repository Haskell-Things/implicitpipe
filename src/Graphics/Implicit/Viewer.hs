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

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Default
import qualified Data.Map

import Graphics.GPipe hiding ((^-^), rotate)
import Graphics.UI.GLFW (WindowHint(..))
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Implicit
import Graphics.Implicit.Primitives (getBox)
import Graphics.Implicit.Export.GL
import Graphics.Implicit.Viewer.Shaders
import Graphics.Implicit.Viewer.Types
import Graphics.Implicit.Viewer.Util

-- | View `SymbolicObj3` object using OpenGL viewer
view :: SymbolicObj3 -> IO ()
view o = viewer $ object o

-- | Animate `SymbolicObj3` object using OpenGL viewer
--
-- Object can be parameterized by animation time `Double`
-- in the interval [0..1].
animate :: (Double -> SymbolicObj3) -> IO ()
animate a = viewer $ animated a

viewerMain :: IO ()
viewerMain = viewer $ rotating def

animateMain :: IO ()
animateMain = viewer $ preview $ zoom 2 $ animated (obj def)

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
    let iniMesh =
            meshToGL
          $ meshFunFromResolution resolution 0 (obj 0)

    triangles :: Buffer os PrimitiveBuffer
      <- newBuffer $ length $ iniMesh

    writeBuffer triangles 0 iniMesh

    win <- newWindow
      (WindowFormatColorDepth RGBA8 Depth16)
      ((GLFW.defaultWindowConfig "GPipe ImplicitCAD viewer")
        { GLFW.configHints = [ WindowHint'Samples $ Just 16 ] }
      )

    -- Try updating scrolling vars
    void . GLFW.setScrollCallback win . pure $
        \dx dy -> do
          void $ tryPutMVar scrollX' (realToFrac dx)
          void $ tryPutMVar scrollY' (realToFrac dy)

    -- Create a buffer for the uniform values
    unionBuffers <- Uniforms
      <$> newBuffer 1
      <*> newBuffer 1
      <*> newBuffer 1
      <*> newBuffer 1

    -- Create the shader
    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream shaderEnvTriangles

      modelViewProj <- getUni bMvpMat
      modelMat      <- getUni bModelMat
      normMat       <- getUni bNormMat
      let
          proj :: (VFloat -> b)
               -> (V3 VFloat, VertexInfo posType (V3 VFloat) barType colorType)
               -> (V4 VFloat, VertexInfo (V3 VFloat) (V3 b) barType colorType)
          proj f (V3 px py pz, VertexInfo{..}) =
            ( modelViewProj !* V4 px py pz 1
            , (VertexInfo {
                viPos = normalizePoint $ modelMat !* V4 px py pz 1
              , viNormal = fmap f $ normMat !* viNormal
              , viBarycentric = viBarycentric
              , viColor       = viColor
              })
            )

      fragmentStream <-
            do
              guard' (shaderEnvFlatNormals)
              rasterize
                shaderEnvRasterOptions
                (proj Flat <$> primitiveStream)
        <|> rasterize
                shaderEnvRasterOptions
                (proj id <$> primitiveStream)

      asumShaderByName shaderEnvFragName win fragmentStream

    -- Run the loop
    loop win shader triangles
      --(Uniforms mvpMatBuffer normMatBuffer eyeBuffer)
      unionBuffers
      ViewerState {
        camYaw            = 0
      , camPitch          = 0
      , camZoom           = initZoom
      , lastCursorPos     = (0, 0)
      , scrollX           = scrollX'
      , scrollY           = scrollY'
      , lastSample        = 0
      , fps               = 0
      , animationRunning  = animation
      , animationForward  = True
      , animationTime     = animationInitTime
      , rotationRunning   = rotation
      , rotationAngle     = rotationInitAngle
      , shouldClose       = False
      , conf              = config
      , objectScale       = (1/s)
      , windowWidth       = 0
      , windowHeight      = 0
      , shaderName        = "default"
      , shaderFlatNormals = True
      }

loop
  :: forall os .  Window os RGBAFloat Depth
  -> (ShaderEnvironment os -> Render os ())
  -> Buffer os PrimitiveBuffer
  -> Uniforms os
  -> ViewerState
  -> ContextT GLFW.Handle os IO ()
loop win shader triangles unionBuffers@Uniforms{..} viewerState = do

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
      newEye = inv44 cameraMatrix !* V4 0 0 0 1
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot

  liftIO $ print (fps, shaderName)
  -- Write this frames uniform values
  writeBuffer bMvpMat   0 [viewProjMat]
  writeBuffer bModelMat 0 [modelMat]
  writeBuffer bNormMat  0 [normMat]
  writeBuffer bEye      0 [normalizePoint newEye]

  triangles' <- case animation of
    False -> return triangles
    True -> do
      let mesh = meshToGL
               $ meshFunFromResolution
                   resolution
                   (realToFrac animationTime)
               $ obj
                   (realToFrac animationTime)

      triangles' <- resizeBuffer triangles (length mesh)
      writeBuffer triangles' 0 mesh
      return triangles'

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane

    vertexArray <- newVertexArray triangles'
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader
      $ ShaderEnvironment
          primitiveArray
          ( FrontAndBack
          , ViewPort 0 (V2 windowWidth windowHeight)
          , DepthRange 0 1
          )
          shaderName
          unionBuffers
          shaderFlatNormals

  swapWindowBuffers win

  unless shouldClose
    $ loop win shader triangles' unionBuffers newViewerState

updateViewerState
  :: forall os .  Window os RGBAFloat Depth
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

  wKey <- GLFW.getKey win GLFW.Key'W
  let nextShader = case wKey of
        Just GLFW.KeyState'Pressed ->
          let cur = Data.Map.findIndex shaderName allShaders
          in  fst
            $ Data.Map.elemAt
                ((cur + 1) `mod` Data.Map.size allShaders)
                allShaders
        _ -> shaderName

  tildeKey <- GLFW.getKey win GLFW.Key'GraveAccent
  let toggleNormals = case tildeKey of
        Just GLFW.KeyState'Pressed -> not shaderFlatNormals
        _ -> shaderFlatNormals

  _mdX <- liftIO $ tryTakeMVar scrollX
  mdY <- liftIO $ tryTakeMVar scrollY
  let newZoom = case mdY of
                  Nothing -> camZoom
                  Just dy -> camZoom + (dy/10)

      animDirection = if animationForward then 1
                                          else -1
      animTime =
        if animationRunning then animationTime + (faster $ animDirection * animationStep)
                     else animationTime

      nextOutOfBounds =
               animationForward && animationTime + animationStep > 1
        || not animationForward && animationTime - animationStep < 0

      rotAngle =
        if rotation then (rotationAngle + faster rotationStep) `mod''` (2*pi)
                    else rotationAngle

  return $ oldViewerState {
      camPitch          = (dPitch + camPitch) `mod''` (2*pi)
    , camYaw            = (dYaw + camYaw) `mod''` (2*pi)
    , camZoom           = newZoom
    , lastCursorPos     = (realToFrac cursorX, realToFrac cursorY)
    , windowWidth       = w
    , windowHeight      = h
    , lastSample        = now
    , fps               = 1 / dt
    , animationRunning  = animationRunning && not nextOutOfBounds || (nextOutOfBounds && animationBounce)
    , animationTime     = animTime
    , animationForward  = (if nextOutOfBounds then not else id) animationForward
    , rotationAngle     = rotAngle
    , shouldClose       = shouldClose'
    , shaderName        = nextShader
    , shaderFlatNormals = toggleNormals
    }
