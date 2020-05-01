{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
module Main where

import Control.Monad
import Text.Printf (printf)
import Control.Monad.IO.Class

import Graphics.GPipe hiding ((^-^))
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import qualified "JuicyPixels" Codec.Picture as Juicy
import qualified "JuicyPixels" Codec.Picture.Types as Juicy

-- aa
import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives
import Graphics.Implicit.Export
import Graphics.Implicit.Export.DiscreteAproxable
--import Graphics.Implicit.Export.Utils
-- XXX: ^^ cleanupTris

import Data.VectorSpace (normalized, (^-^))
import Data.Cross (cross3)

import GHC.Float (double2Float)

import qualified Linear as L

trim :: NormedTriangleMesh
trim = cleanupTris . discreteAprox 1.5 $ obj

obj = autocenter $ union $ [
    rect3R 0 (0,0,0) (20,20,20)
  , translate (20,20,20) (sphere 15)
  , translate (0,0,25) (rect3R 2 (0,0,0) (10,10,10))
  --, translate (0, 0, -30) fun
  ] ++
  []
  --map (\x -> translate (0,0,20 + (10 - x) * 11) (rect3R 0 (0,0,0) (10 - x, 10 - x, 10 - x))) [0..9]

fun:: SymbolicObj3
fun = implicit (\(x,y,z) -> x^4 + y^4 + z^4 - 15000) ((-20,-20,-20),(20,20,20))

ptri (NormedTriangleMesh ts) = mapM_ (\(NormedTriangle (a, b, c)) -> print (a, b, c) ) ts

--rmp (NormedTriangleMesh ts) = concatMap (\(NormedTriangle ((a, na), (b, nb), (c, nc))) -> [v a, v b, v c] ) ts
rmp s (NormedTriangleMesh ts) = concatMap (\(NormedTriangle ((a, na), (b, nb), (c, nc))) -> [v s a na, v s b nb, v s c nc] ) ts
--rmp (NormedTriangleMesh ts) = concatMap (\(NormedTriangle (a, b, c)) -> [v a, v b, v c] ) ts

--v (x, y, z) = double2Float <$> V3 x y z
--v (x, y, z) = (double2Float . sc <$> V3 x y z, V3 0 1 0)
v scale (x, y, z) (xn, yn, zn) =
    (chk $ dtf $ sc scale $ pack (x, y, z), cvt (xn, yn, zn))
  --(double2Float . sc scale <$> V3 x y z, double2Float <$> V3 xn yn zn)
--v scale a b = (double2Float . sc scale <$> a, double2Float <$> b)

sc :: (Fractional a) => V3 a -> V3 a -> V3 a
sc (V3 byx byy byz) (V3 x y z) = V3 (x / byx) (y / byy) (z / byz)

chk v@(V3 x y z) = if x < -1.0 || y < -1.0 || z < -1.0 then error $  "<1" ++ show v
  else if x > 1.0 || y > 1.0 || z > 1.0 then error $  ">1" ++ show v
  else V3 x y z

cvt :: (Double, Double, Double)-> V3 Float
cvt a = double2Float <$> pack a

dtf a = double2Float <$> a

pack :: (Double, Double, Double) -> V3 Double
pack (x, y, z) = V3 x y z

unmesh :: NormedTriangleMesh -> [NormedTriangle]
unmesh (NormedTriangleMesh m) = m

normal :: (ℝ3,ℝ3,ℝ3) -> ℝ3
normal (a,b,c) =
    normalized $ (b ^-^ a) `cross3` (c ^-^ a)

-- | Removes triangles that are empty when converting their positions to Float resolution.
cleanupTris :: NormedTriangleMesh -> NormedTriangleMesh
cleanupTris tris =
    let 
        --floatPoint :: (ℝ, ℝ, ℝ) -> (Float, Float, Float)
        --floatPoint (a,b,c) = (toFloat a, toFloat b, toFloat c)
        --floatPoint a = double2Float <$> a
        floatPoint (a,b,c) = (double2Float a, double2Float b, double2Float c)

        -- | Does this triangle fail because it is constrained on two axises?
        isDegenerateTri2Axis :: Eq a => ((a, a, a),(a, a, a),(a, a, a)) -> Bool
        isDegenerateTri2Axis tri = (ysame tri && xsame tri) || (zsame tri && ysame tri) || (zsame tri && xsame tri)
          where
            same :: Eq a => (a, a, a) -> Bool
            same (n1, n2, n3) = n1 == n2 && n2 == n3
            xsame :: Eq a => ((a, a, a), (a, a, a), (a, a, a)) -> Bool
            xsame ((x1,_,_),(x2,_,_),(x3,_,_)) = same (x1, x2, x3) 
            ysame :: Eq a => ((a, a, a), (a, a, a), (a, a, a)) -> Bool
            ysame ((_,y1,_),(_,y2,_),(_,y3,_)) = same (y1, y2, y3) 
            zsame :: Eq a => ((a, a, a), (a, a, a), (a, a, a)) -> Bool
            zsame ((_,_,z1),(_,_,z2),(_,_,z3)) = same (z1, z2, z3) 
        isDegenerateTri :: NormedTriangle -> Bool
        isDegenerateTri (NormedTriangle ((a, _), (b, _), (c, _))) = isDegenerateTri2Axis floatTri
          where
            floatTri = (floatPoint a, floatPoint b, floatPoint c)
    in NormedTriangleMesh $ filter (not . isDegenerateTri) (unmesh tris)

autocenter obj = let (a, b) = getBox obj in translate ((a - b) / 2) obj

bmax (x, y, z) = maximum [x, y, z]

main = do
  --ptri trim
  --print "WTF"
  let (ba, bb) = getBox obj
      (bax, _, _) = ba
      (bbx, _, _) = bb
      -- ratio perserving scaling
      s = let m = bmax (bb - ba) in pack (m, m, m)  -- ^* 1.0
      -- scale by x
      --s = V3 (bbx - bax) (bbx - bax) (bbx - bax)
      -- non-preserving scaling
      --s = pack $ bb - ba

  print (ba, bb, s)
  --print $ rmp s trim
  runContextT GLFW.defaultHandleConfig $ do
    -- Create vertex data buffers
    tris      :: Buffer os (B3 Float, B3 Float) <- newBuffer $ (*4) $ length $ unmesh trim
    positions :: Buffer os (B2 Float) <- newBuffer 4
    normals   :: Buffer os (B3 Float) <- newBuffer 6
    tangents  :: Buffer os (B3 Float) <- newBuffer 6

    writeBuffer tris 0 $ rmp s trim
    writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
    writeBuffer normals 0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
    writeBuffer tangents 0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]

    win <- newWindow (WindowFormatColorDepth SRGB8 Depth16) (GLFW.defaultWindowConfig "test it!")

    -- Spew scroll info
    void . GLFW.setScrollCallback win . pure $
        \dx dy -> printf "scroll dx%v dy%v\n" dx dy

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
      --tries <- toPrimitiveStream mytris

      primitiveStream <- toPrimitiveStream mytris

      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let
          projected = proj modelViewProj normMat <$> primitiveStream

      let filterMode = SamplerFilter Linear Linear Linear (Just 4)
          edgeMode = (pure ClampToEdge, undefined)

      samp <- newSampler2D (const (tex, filterMode, edgeMode))

      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 (1920 `div` 1) 1080), DepthRange 0 1)) projected -- primitiveStream
      -- colorful normals
      --drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

      let fragNormals = fragmentStream
      let --litFrags = fragNormals
          litFrags = go . light samp <$> fragNormals
          litFragsWithDepth = withRasterizedInfo
              (\a x -> (a, getZ $ rasterizedFragCoord x)) litFrags
          colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

      return ()

    -- Run the loop
    loop win shader makePrimitives tris uniform 0

loop win shader makePrimitives tris uniform angleRot = do
  --Just (cursorX, cursorY) <- GLFW.getCursorPos win
  mouseButton1 <- GLFW.getMouseButton win GLFW.MouseButton'1
  spaceKey <- GLFW.getKey win GLFW.Key'Space
  shouldClose <- GLFW.windowShouldClose win
--liftIO $ printf "cursorPos x%v y%v, mouseButton1 %v, spaceKey %v, shouldClose %v\n"
--  cursorX cursorY (show mouseButton1) (show spaceKey) (show shouldClose)
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize win
  let modelRot = fromQuaternion (axisAngle (V3 1 0.5 0.3) angleRot)
      modelMat = mkTransformationMat modelRot (pure 0)
      projMat = perspective (pi/3) (fromIntegral w / fromIntegral h) 1 100
      --viewMat = mkTransformationMat identity (- V3 0 0 3)
      --
      cameraMatrix :: Float -> L.M44 Float
      cameraMatrix t = lookAt eye (V3 0 0 0) (V3 0 1 0)
        where eye = L.rotate (axisAngle (V3 0 1 0) (t / 10)) (V3 1 1 1)
      viewMat = cameraMatrix 0
      viewProjMat = projMat !*! viewMat !*! modelMat
      --normMat = -identity -- !*! modelRot
      --normMat = identity -- !*! modelRot
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane
    prims <- makePrimitives

    --vertexArray <- newVertexArray vertexBuffer
    --let primitiveArray = toPrimitiveArray TriangleList vertexArray
    vertexArray <- newVertexArray tris
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader $ ShaderEnvironment prims primitiveArray (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapWindowBuffers win

  cr <- GLFW.windowShouldClose win
  case cr of
    Nothing -> return ()
    Just closeRequested -> do
      unless closeRequested $
        loop win shader makePrimitives tris uniform ((angleRot + 0.005) `mod''` (2*pi))
        --loop win shader makePrimitives tris uniform angleRot

getJuicyPixel xs _x _y pix =
  let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs

getZ (V4 _ _ z _) = z -- Some day I'll learn to use lenses instead...

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (B2 Float, (B3 Float, B3 Float))
  , mytris :: PrimitiveArray Triangles (B3 Float, B3 Float)
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

proj modelViewProj normMat (V3 px py pz, normal) =
  (modelViewProj !* V4 px py pz 1, fmap Flat $ normMat !* normal)

light color normal =
  (V3 0 0.5 0) ^* (clamp (normal `dot` V3 0 1 1) 0 1)
-- ^^ light color

-- global ilum hack
go color = color ^+^ 0.2 -- 0.1 ^+^ color ^+^ V3 0.2 0 0

-- eof
--
-- OLDstuff
--
-- Set color from sampler and apply directional light
light'' samp normal =
  --sample2D samp SampleAuto Nothing Nothing $ pure (normal `dot` V3 0 0 1)
  pure (normal `dot` V3 0 0 1)

-- Project the cube's positions and normals with ModelViewProjection matrix
proj' modelViewProj normMat (V3 px py pz, normal, uv) =
  (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv))

-- Project the sides coordinates using the instance's normal and tangent
makeSide (p@(V2 x y), (normal, tangent)) =
  (V3 x y 1 *! V3 tangent bitangent normal, normal, uv)
  where bitangent = cross normal tangent
        uv = (p + 1) / 2

light' samp (normal, uv) =
  sample2D samp SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1)


