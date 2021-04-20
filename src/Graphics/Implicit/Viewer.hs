{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Implicit.Viewer (
    animate
  , view
  , viewer
  , ViewerConf(..)
  , parseViewerConf
  , animateMain
  , eval
  , def
  ) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import Data.Default
import qualified Data.Map

import Graphics.GPipe hiding (mod', rotate, (^-^))
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Graphics.UI.GLFW (WindowHint(..))

import Graphics.Implicit
import Graphics.Implicit.Viewer.Demos
import Graphics.Implicit.Viewer.Loaders
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

animateMain :: IO ()
animateMain = viewer $ preview $ zoom 2 $ animated demoLetterI

-- | Configurable viewer
--
-- Same viewer is used for both static viewer
-- and animation.
--
viewer :: ViewerConf -> IO ()
viewer config@ViewerConf{..} = do
  eventChan  <- newTChanIO :: IO (TChan Event)
  renderChan <- newTChanIO :: IO (TChan Rendered)
  animationTimeVar <- newTVarIO 0 :: IO (TVar Double)

  case obj of
    Just o  -> do
      renderObjToChan (o 0) initResolution renderChan
      when animation $ runAnimation o initResolution renderChan animationTimeVar
    Nothing -> return ()
  case moduleFile of
    Just f  -> runUpdater f initResolution renderChan
    Nothing -> return ()

  runContextT GLFW.defaultHandleConfig $ do
    triangles :: Buffer os PrimitiveBuffer
      <- newBuffer 0

    win <- newWindow
      (WindowFormatColorDepth RGBA8 Depth16)
      ((GLFW.defaultWindowConfig "GPipe ImplicitCAD viewer")
        { GLFW.configHints = [ WindowHint'Samples $ Just 16 ] }
      )

    setupCallbacks win eventChan
    -- Get and forward window size right after initialization
    -- so we don't wait for callback to set it
    ws <- GLFW.getWindowSize win
    case ws of
      Just (w, h) -> liftIO $ atomically $ writeTChan eventChan $ WindowSize $ V2 w h
      _           -> return ()

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
              guard' shaderEnvFlatNormals
              rasterize
                shaderEnvRasterOptions
                (proj Flat <$> primitiveStream)
        <|> rasterize
                shaderEnvRasterOptions
                (proj id <$> primitiveStream)

      asumShaderByID shaderEnvFragID win fragmentStream

    -- Run the loop
    loop
      win
      shader
      triangles
      unionBuffers
      animationTimeVar
      eventChan
      renderChan
      ViewerState {
        camYaw            = 0
      , camPitch          = 0
      , camZoom           = initZoom
      , camRotating       = False
      , lastCursorPos     = pure 0
      , lastSample        = 0
      , fps               = 0
      , animationRunning  = animation
      , animationForward  = True
      , animationTime     = animationInitTime
      , rotationRunning   = rotation
      , rotationAngle     = rotationInitAngle
      , shouldClose       = False
      , conf              = config
      , objectScale       = Nothing
      , autoScale         = False
      , windowSize        = pure 0
      , shaderID          = 0
      , shaderFlatNormals = True
      }

loop
  :: forall os .  Window os RGBAFloat Depth
  -> (ShaderEnvironment os -> Render os ())
  -> Buffer os PrimitiveBuffer
  -> Uniforms os
  -> TVar Double
  -> TChan Event
  -> TChan Rendered
  -> ViewerState
  -> ContextT GLFW.Handle os IO ()
loop win shader triangles unionBuffers@Uniforms{..} aTime eventChan renderChan viewerState = do
  (triangles', scaledViewerState) <- do
    mx <- liftIO $ atomically $ tryReadTChan renderChan
    case mx of
      Just (len, objScale, tris) -> do
        triangles' <- resizeBuffer triangles len
        writeBuffer triangles' 0 tris
        return (triangles', case autoScale viewerState of
          True -> viewerState { objectScale = Just $ 1 / objScale }
          False -> viewerState { objectScale = Just $ maybe (1 / objScale) id (objectScale viewerState) })
      Nothing -> do
        return (triangles, viewerState)

  newViewerState@ViewerState{..} <- updateViewerState win eventChan scaledViewerState

  -- update TVar so runAnimation can grab it
  liftIO $ atomically $ writeTVar aTime (realToFrac animationTime)

  let ViewerConf{..} = conf

  let V2 windowWidth windowHeight = windowSize
      modelRot = fromQuaternion (axisAngle (V3 0 0 1) (-rotationAngle))

      modelMat =
            mkTransformationMat modelRot (pure 0)
        !*! mkScaleTransform (maybe 1 id objectScale)

      projMat = perspective (pi/2) (fromIntegral windowWidth / fromIntegral windowHeight) 0.1 100

      eye = V3 0 (-1) 1
      lookAtPoint = V3 0 0 0

      cameraMatrix :: M44 Float
      cameraMatrix =
        lookAt
          eye
          lookAtPoint
          (V3 0 0 1) -- up vector
        !*!
        mkTransformation (
            axisAngle (V3 1 0 0) (camPitch)
          * axisAngle (V3 0 0 1) (camYaw)
          )
          (pure 0)
        !*!
        mkScaleTransform camZoom

      viewMat = cameraMatrix
      newEye = inv44 cameraMatrix !* V4 0 0 0 1
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot

  when debug $ liftIO $ putStrLn $ "FPS: " ++ show fps
  -- Write this frames uniform values
  writeBuffer bMvpMat   0 [viewProjMat]
  writeBuffer bModelMat 0 [modelMat]
  writeBuffer bNormMat  0 [normMat]
  writeBuffer bEye      0 [normalizePoint newEye]

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
          , ViewPort 0 windowSize
          , DepthRange 0 1
          )
          shaderID
          unionBuffers
          shaderFlatNormals

  swapWindowBuffers win

  unless shouldClose
    $ loop win shader triangles' unionBuffers aTime eventChan renderChan newViewerState

updateViewerState
  :: forall os .  Window os RGBAFloat Depth
  -> TChan Event
  -> ViewerState
  -> ContextT GLFW.Handle os IO ViewerState
updateViewerState win chan oldState = do
  let ViewerConf{..} = conf oldState

  (Just now) <- liftIO $ GLFW.getTime
  let dt = realToFrac now - lastSample oldState
      V2 oldCursorX oldCursorY = lastCursorPos oldState

  let handleEvents s@ViewerState{..} = do
        emptyChan <- liftIO $ atomically $ isEmptyTChan chan
        if emptyChan then return s
        else do
          msg <- liftIO $ atomically $ readTChan chan
          handleEvents $ case msg of
            Cursor x
              -> case camRotating of
                  False -> s { lastCursorPos = x }
                  True ->
                    let
                      V2 cursorX cursorY = lastCursorPos
                    in
                      s { lastCursorPos = x
                        , camPitch = (realToFrac (cursorY - oldCursorY) / 100 + camPitch) `mod''` (2*pi)
                        , camYaw = (realToFrac (cursorX - oldCursorX) / 100 + camYaw) `mod''` (2*pi)
                        }
            LeftMouse x
              -> s { camRotating = x }
            SwitchShader to
              -> s { shaderID = to }
            NextShader
              -> s { shaderID = nextInMap shaderID allShaders }
            ToggleFlatNormals
              -> s { shaderFlatNormals = not shaderFlatNormals }
            ToggleAutoRotate
              -> s { rotationRunning = not rotationRunning }
            ToggleAutoScale
              -> s { autoScale = not autoScale }
            Zoom z
              -> s { camZoom = camZoom + (z/10) }
            WindowSize x
              -> s { windowSize = x }
            Quit
              -> s { shouldClose = True }

  newState@ViewerState{..} <- handleEvents oldState

  spaceKey <- GLFW.getKey win GLFW.Key'Space
  let faster = case spaceKey of
        Just GLFW.KeyState'Pressed -> (*10)
        _                          -> id

      animDirection = if animationForward then 1
                                          else -1
      animTime =
        if animationRunning
             then animationTime + faster (animDirection * animationStep)
             else animationTime

      nextOutOfBounds =
               animationForward && animationTime + animationStep > 1
        || not animationForward && animationTime - animationStep < 0

      rotAngle =
        if rotationRunning
             then (rotationAngle + faster rotationStep) `mod''` (2*pi)
             else rotationAngle

  return $ newState
    { lastSample        = realToFrac now
    , fps               = 1 / dt
    , animationRunning  = animationRunning && not nextOutOfBounds || (nextOutOfBounds && animationBounce)
    , animationTime     = animTime
    , animationForward  = (if nextOutOfBounds then not else id) animationForward
    , rotationAngle     = rotAngle
    }

-- | Setup GLFW callbacks pumping events to event channel
setupCallbacks
 :: MonadIO m
 => Window os c ds
 -> TChan Event
 -> ContextT GLFW.Handle os m ()
setupCallbacks win chan = do
  void $ GLFW.setWindowSizeCallback win . pure $
    \w h -> do
      atomically $ writeTChan chan $ WindowSize $ V2 w h

  void $ GLFW.setWindowCloseCallback win . pure $
    atomically $ writeTChan chan Quit

  void $ GLFW.setScrollCallback win . pure $
    \_dx dy -> do
      atomically $ writeTChan chan $ Zoom $ realToFrac dy

  void $ GLFW.setCursorPosCallback win . pure $
    \x y -> do
      atomically $ writeTChan chan $ Cursor (V2 x y)

  void $ GLFW.setMouseButtonCallback win . pure $
    \button buttonState _modifierKeys -> do
      when (button == GLFW.MouseButton'1) $ do
        atomically $ writeTChan chan $ LeftMouse
          (buttonState == GLFW.MouseButtonState'Pressed)

  let keyMap = Data.Map.fromList $
        [ (GLFW.Key'N           , ToggleFlatNormals)
        , (GLFW.Key'GraveAccent , ToggleFlatNormals)
        , (GLFW.Key'C           , ToggleAutoScale)
        , (GLFW.Key'R           , ToggleAutoRotate)
        , (GLFW.Key'Q           , Quit)
        , (GLFW.Key'Tab         , NextShader)
        ]
        ++
        (zip
          [ GLFW.Key'1 .. GLFW.Key'9 ]
          $ map SwitchShader (Data.Map.keys allShaders))

  void $ GLFW.setKeyCallback win . pure $
    \key _i keyState _modifierKeys -> do
      when (keyState == GLFW.KeyState'Pressed) $ do
        case Data.Map.lookup key keyMap of
          Just message -> atomically $ writeTChan chan message
          _            -> return ()
