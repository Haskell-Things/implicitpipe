{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Graphics.GPipe hiding ((^-^), rotate)
import Graphics.UI.GLFW (WindowHint(..))
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives
import Graphics.Implicit.Export.DiscreteAproxable

objG t = rotate3 (0, 0, t*2*pi) $ cylinder2 2 1 2 `ontop` rect3R 0 (-3,-3,-1) (3,3,1)


demo3 = const $ cylinder2 2 1 2 `ontop` rect3R 0 (-3,-3,-1) (3,3,1)

ontop a b = union [ translate (0, 0, z) a, b ]
  where z =
              let
                  ((_, _, aBottom), _) = getBox a
                  (_, (_ ,_ , bTop)) = getBox b
              in bTop - aBottom


-- demo
-- XXX: refactor to animate and t param from [0, 1]
objD :: Double -> SymbolicObj3
objD s' = union $ [
   translate (s*7,0,0) (sphere (5 - s*(7/5)))
 , translate (-(s*7),0,0) (sphere (5 - s*(7/5)))
 , translate (-5-(s*(7/2)), 0, 0) $ rotateExtrude (s * 360) (Just 0) (Left (0, 2)) (Left 0) (translate (4, 0) $ circle 1)
 , translate (-10, 20, 0)
    $ rotate3 (0, s * 2 * pi, 0)
      $ union [
          translate (0, 0, -s) (cylinder2 1 3 7)
        , translate (0, 0, s) $ rotate3 (pi, 0, 0) $  cylinder2 1 3 7 ]
 ] ++
   map (\x -> translate (5 + (fromIntegral x * 2), 5, 0)
    (rect3R 0 (0,0,0) (1, 1, fromIntegral x))
    ) [(0 :: Int)..(round s)]
  where s = s' + 0.01 -- noglitchart

obj1 s = union $ [
   rect3R 0 (0,0,0) (20,20,20)
 -- , translate (20,20,20) (sphere 15)
 -- , translate (30,20,20) (sphere 5)
 -- , translate (0,0,25) (rect3R 2 (0,0,0) (10,10,10))
 -- , translate (25,0,0) (cylinder2 10 1 100)
  , translate (45,0,0) $ difference [
        cylinder 5 s
      , cylinder 3 (s+2)
      ]
  --, translate (45, 0, 0) fun
  ] ++
  []
  --map (\x -> translate (0,0,20 + (10 - x) * 11) (rect3R 0 (0,0,0) (10 - x, 10 - x, 10 - x))) [0..9]

objF = translate (45, 0, 0) fun

fun :: SymbolicObj3
fun = implicit (\(x,y,z) -> x^(4 :: Int) + y^(4 :: Int) + z^(4 :: Int) - 15000) ((-20,-20,-20),(20,20,20))

objToGL
  :: Double
  -> SymbolicObj3
  -> [(V3 Float, V3 Float)]
objToGL resolution object =
    meshToGL
  . discreteAprox resolution
  $ object

meshToGL
  :: NormedTriangleMesh
  -> [(V3 Float, V3 Float)]
meshToGL (NormedTriangleMesh ts) =
  concatMap
    (\(NormedTriangle (a, b, c)) -> map toV3Pair [a, b, c]) ts
  where
    toV3Pair ((x, y, z), (xn, yn, zn)) =
      ( realToFrac <$> V3 x y z
      , realToFrac <$> V3 xn yn zn)

type UniformBuffer = (V4 (B4 Float), V3 (B3 Float))
type PrimitiveBuffer = (B3 Float, B3 Float)

data ShaderEnvironment = ShaderEnvironment
  { shaderEnvTriangles     :: PrimitiveArray Triangles PrimitiveBuffer 
  , shaderEnvRasterOptions :: (Side, ViewPort, DepthRange)
  }

-- Resolution can be a function of time
-- e.g.:
-- Varied (discreteAprox . (+0.1) . (*5))

data Resolution =
    Fixed Double
  | Varied (Double -> SymbolicObj3 -> NormedTriangleMesh)

apResolution f (Fixed n) = Fixed (f n)
apResolution f (Varied f') = Varied $ f' . f


data ViewerConf = ViewerConf {
    obj               :: Double -> SymbolicObj3
  , resolution        :: Resolution
  , animation         :: Bool
  , animationBounce   :: Bool
  , animationInitTime :: Float
  , animationStep     :: Float
  , rotation          :: Bool
  , rotationStep      :: Float
  , rotationInitAngle :: Float
  , fudgeBufferSize   :: Int -> Int
  }

meshFunFromResolution (Fixed n) = const $ discreteAprox n
meshFunFromResolution (Varied f) = f

main = genericViewer $ preview $ animate $ defaultConf

demoLetterI t =
    scale 2
  $ translate (0, 0, -7)
  $ unionR 1
  [ rect3R 1 (-2,-2,0) (2,2,10)
  , translate (0, 0, 13 - (13 * t)) $ sphere (3 + t)
  ]


defaultConf = ViewerConf
  { obj               = demoLetterI
  , resolution        = Fixed 1
  , animation         = False
  , animationBounce   = True
  , animationInitTime = 0
  , animationStep     = 0.001
  , rotation          = False
  , rotationStep      = 0.001
  , rotationInitAngle = 0
  , fudgeBufferSize   = id
  }

animate c = c {
    animation = True
  , fudgeBufferSize = (*10)
  }

preview c = c {
    resolution    = apResolution (*10) (resolution c)
  , animationStep = animationStep c * 10
  , rotationStep  = rotationStep c * 10
  }


genericViewer :: ViewerConf -> IO ()
genericViewer config@ViewerConf{..} = do
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

      -- colorful weird normals
      -- drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

      let
          getZ (V4 _ _ z _) = z

          litFrags = light <$> fragmentStream
          litFragsWithDepth = withRasterizedInfo
              (\a x -> (a, getZ $ rasterizedFragCoord x)) litFrags
          colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawWindowColorDepth
        (const (win, colorOption, depthOption))
        litFragsWithDepth

      return ()

    let vs = ViewerState {
        camYaw           = 0
      , camPitch         = 0
      , camZoom          = 1
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
    -- Run the loop
    loop win shader triangles uniform vs

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

updateViewer
  :: forall os .  Window os RGBFloat Depth
  -> ViewerState
  -> ContextT GLFW.Handle os IO ViewerState
updateViewer win oldViewerState@ViewerState{..} = do
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

loop
  :: forall os .  Window os RGBFloat Depth
  -> (ShaderEnvironment -> Render os ())
  -> Buffer os PrimitiveBuffer
  -> Buffer os (Uniform UniformBuffer)
  -> ViewerState
  -> ContextT GLFW.Handle os IO ()
loop win shader triangles uniform viewerState = do

  newViewerState@ViewerState{..} <- updateViewer win viewerState
  let ViewerConf{..} = conf

  let modelRot = -- identity
        fromQuaternion (axisAngle (V3 0 0 1) (-rotationAngle))

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

mkScaleTransform :: Float -> M44 Float
mkScaleTransform s =
  (V4 (V4 s 0 0 0)
      (V4 0 s 0 0)
      (V4 0 0 s 0)
      (V4 0 0 0 1)
      )
