{-# LANGUAGE ScopedTypeVariables, TypeFamilies, PackageImports #-}
module Main where

import qualified Control.Monad.IO.Class as MIO
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadException)

import qualified Graphics.GPipe.Context as C
import qualified Graphics.GPipe.Buffer as B
import qualified Graphics.GPipe.Format as F
import qualified Graphics.GPipe.FragmentStream as FS
import qualified Graphics.GPipe.FrameBuffer as FB
import qualified Graphics.GPipe.PrimitiveArray as PA
import qualified Graphics.GPipe.PrimitiveStream as PS
import qualified Graphics.GPipe.Shader as S

import qualified Graphics.GPipe.Context.GLFW as Ctx
import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

main :: IO ()
main = C.runContextT Ctx.newContext (F.ContextFormatColor F.RGB8) $ do
    myBuffer :: B.Buffer os (B.B3 Float) <- B.newBuffer 3
    shader <- S.compileShader myShader
    myLoop shader myBuffer

type MyShader os ds = S.Shader
    os
    (F.ContextFormat F.RGBFloat ds)
    (FS.ViewPort, PA.PrimitiveArray PA.Triangles (B.B3 Float))
    ()

myShader :: MyShader os ds
myShader = do
    primitives <- PS.toPrimitiveStream snd
    frags <- FS.rasterize
        (\(v,_) -> (FS.FrontAndBack, v, FS.DepthRange 0 1))
        (fmap (\(x,y,z) -> ((x,y,z,1),())) primitives)
    FB.drawContextColor
        (fmap (const (255,255,255)) frags)
        (const (FB.ContextColorOption FB.NoBlending (True,True,True)))

myLoop ::
    ( MonadException m
    , MonadIO m
    , F.ContextColorFormat c
    , Num rr , Num gg , Num bb
    , F.Color c Float ~ (rr, gg, bb)
    , Fractional t , Fractional t1 , Fractional t2
    , B.HostFormat a ~ (t, t1, t2)
    )
    => ( (FS.ViewPort, PA.PrimitiveArray PA.Triangles a)
       -> C.Render os (F.ContextFormat c ds) ()
       )
    -> B.Buffer os a
    -> C.ContextT os (F.ContextFormat c ds) m b
myLoop s b = do
    B.writeBuffer [(-0.9,-0.9,0.3), (0.0,0.9,0.3),(0.9,-0.9,0.3)] 0 b
    Just t <- MIO.liftIO GLFW.getTime
    MIO.liftIO $ print t
    C.render $ myRender t s b
    C.swapContextBuffers
    myLoop s b

myRender :: forall b os c ds t t1 t2 a.
    ( F.ContextColorFormat c
    , Num t2, Num t1, Num t
    , F.Color c Float ~ (t, t1, t2)
    )
    => Double
    -> ( (FS.ViewPort, PA.PrimitiveArray PA.Triangles a)
       -> C.Render os (F.ContextFormat c ds) b
       )
    -> B.Buffer os a
    -> C.Render os (F.ContextFormat c ds) b

myRender now s b = do
    FB.clearContextColor (0,0,0)
    size <- C.getContextBuffersSize
    varr <- PA.newVertexArray b
    s (FS.ViewPort (0,0) size, PA.toPrimitiveArray PA.TriangleList varr)

-- eof
