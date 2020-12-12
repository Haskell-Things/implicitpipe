
module Graphics.Implicit.Viewer.Config where

import Data.Default
import Options.Applicative

import Graphics.Implicit.Definitions
import Graphics.Implicit.Export.DiscreteAproxable
import Graphics.Implicit.Viewer.Demos

-- | Rendering resolution
--
-- @Fixed@ (typically in millimeters) or
-- a function of time:
--
-- @Varied (discreteAprox . (+0.1) . (*5))@
data Resolution =
    Fixed Double
  | Varied (Double -> SymbolicObj3 -> NormedTriangleMesh)

-- | Scale resolution by function
apResolution :: (Double -> Double) -> Resolution -> Resolution
apResolution f (Fixed n) = Fixed (f n)
apResolution f (Varied f') = Varied $ f' . f

meshFunFromResolution
  :: Resolution
  -> Double
  -> SymbolicObj3
  -> NormedTriangleMesh
meshFunFromResolution (Fixed n) = const $ discreteAprox n
meshFunFromResolution (Varied f) = f

data ViewerConf = ViewerConf
  { obj               :: Maybe (Double -> SymbolicObj3)
                                        -- ^ Time parametrized object
  , moduleFile        :: Maybe FilePath -- ^ Path to .hs or .escad file to load
  , initResolution    :: Resolution     -- ^ Initial rendering resolution
  , initZoom          :: Float          -- ^ Initial zoom
  , animation         :: Bool           -- ^ Enable animation
  , animationBounce   :: Bool           -- ^ Animation bounces (reverses direction at the limits)
  , animationInitTime :: Float          -- ^ Initial animation time [0..1]
  , animationStep     :: Float          -- ^ Animation time step size
  , rotation          :: Bool           -- ^ Enable rotation
  , rotationStep      :: Float          -- ^ Rotation step size
  , rotationInitAngle :: Float          -- ^ Initial rotation angle (in radians)
  , debug             :: Bool           -- ^ Print debugging info
  }

instance Default ViewerConf where
  def = ViewerConf
    { obj               = Just demoLetterI
    , moduleFile        = Nothing
    , initResolution    = Fixed 1
    , initZoom          = 1
    , animation         = False
    , animationBounce   = True
    , animationInitTime = 0
    , animationStep     = 0.001
    , rotation          = False
    , rotationStep      = 0.001
    , rotationInitAngle = 0
    , debug             = False
    }

-- | Default config with object
object :: SymbolicObj3 -> ViewerConf
object x = def { obj = Just $ const x }

-- | Default config with animation
animated :: (Double -> SymbolicObj3) -> ViewerConf
animated ft = def
  { obj = Just ft
  , animation = True }

zoom :: Float -> ViewerConf -> ViewerConf
zoom n x = x { initZoom = n }

-- | Enable preview mode with lower resolution
-- and higher animation and rotation step sizes.
preview :: ViewerConf -> ViewerConf
preview c = c {
    initResolution = apResolution (*2) (initResolution c)
  , animationStep  = animationStep c * 10
  , rotationStep   = rotationStep c * 10
  }

-- | Enable object rotation
rotating :: ViewerConf -> ViewerConf
rotating c = c { rotation = True }

viewerConfParser :: Parser ViewerConf
viewerConfParser = ViewerConf
  <$> pure Nothing
  <*> (Just <$> strArgument
        (  metavar "FILE"
        <> help ".hs or .escad file to render"
        ))
  <*> (Fixed <$> option auto
        (  long "resolution"
        <> short 'r'
        <> showDefault
        <> value 1
        <> help "Initial resolution to render with, can be overriden in loaded file"
        ))
  <*> option auto
        (  long "zoom"
        <> short 'z'
        <> showDefault
        <> value 1
        <> help "Initial zoom"
        )
  <*> pure False -- animation
  <*> pure True  -- animationBounce
  <*> pure 0     -- animationInitTime
  <*> pure 0.001 -- animationStep
  <*> switch
        ( long "rotate"
        <> help "Whether to enable autorotation" )
  <*> option auto
        ( long "rotate-step"
        <> value 0.001
        <> showDefault
        <> help "Autorotation increment"
        )
  <*> option auto
        ( long "rotate-init"
        <> value 0
        <> showDefault
        <> help "Initial rotation in radians"
        )
  <*> switch
        ( long "debug"
        <> short 'd'
        <> help "Whether to print debugging messages" )

parseViewerConf :: IO ViewerConf
parseViewerConf = execParser opts
  where
    opts = info (viewerConfParser <**> helper)
      ( fullDesc
     <> progDesc "implicitview - view and dynamically reload objects from files"
     <> header "OpenGL (GPipe) based viewer for ImplicitCAD" )
