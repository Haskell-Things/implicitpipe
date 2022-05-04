module Anim where

import Ease
import           Data.Fixed                 (mod')

-- | Duration of an animation or effect. Usually measured in seconds.
type Duration = Double
-- | Time signal. Goes from 0 to 1, inclusive.
type Time = Double
-- | Animations are symbolic objects over a finite time.
data Animation a = Animation Duration (Time -> a)

-- | Query the duration of an animation.
duration :: Animation a -> Duration
duration (Animation d _) = d

-- | Construct an animation with a given duration.
mkAnimation :: Duration -> (Time -> a) -> Animation a
mkAnimation = Animation

instance (Semigroup a) => Semigroup (Animation a) where
  (Animation d1 a1) <> (Animation d2 a2) = Animation (d1 + d2)
    $ \t -> if t < d1/d
       then a1 (t * d/d1)
       else a2 ((t-d1/d) * d/d2)
    where d = d1 + d2

instance (Monoid a) => Monoid (Animation a) where
  mempty = Animation 0 mempty

instance (Show a) => Show (Animation a) where
  show (Animation d x) = show (d, x 0)

-- | Construct an animation with a duration of @1@.
anim :: (Time -> a) -> Animation a
anim = Animation 1

-- | Play two animation concurrently. Shortest animation freezes on last frame.
--   New animation duration is '@max (duration lhs) (duration rhs)@'.
par :: Semigroup a => Animation a -> Animation a -> Animation a
par (Animation d1 f1) (Animation d2 f2) =
  Animation (max d1 d2) $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2
    in f1 (min 1 t1)
    <> f2 (min 1 t2)
  where
    totalD = max d1 d2

-- | Play two animation concurrently. Shortest animation loops.
--   New animation duration is '@max (duration lhs) (duration rhs)@'.
parLoop :: Semigroup a => Animation a -> Animation a -> Animation a
parLoop (Animation d1 f1) (Animation d2 f2) =
  Animation (max d1 d2) $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2
    in f1 (t1 `mod'` 1)
    <> f2 (t2 `mod'` 1)
  where
    totalD = max d1 d2


-- | Play two animation concurrently. Animations disappear after playing once.
--   New animation duration is '@max (duration lhs) (duration rhs)@'.
parDrop :: Monoid a => Animation a -> Animation a -> Animation a
parDrop (Animation d1 f1) (Animation d2 f2) =
  Animation totalD $ \t ->
    let t1 = t * totalD/d1
        t2 = t * totalD/d2
    in (if t1>1 then mempty else f1 t1)
    <> (if t2>1 then mempty else f2 t2)
  where
    totalD = max d1 d2

pause :: Monoid a => Duration -> Animation a
pause d = Animation d $ const mempty

andThen :: Monoid a => Animation a -> Animation a -> Animation a
andThen a b = a `par` (pause (duration a) <> b)

frameAt :: Time -> Animation a -> a
frameAt t (Animation d f) = f t'
  where
    t' = clamp 0 1 (t/d)

clamp :: Double -> Double -> Double -> Double
clamp a b number
  | a < b     = max a (min b number)
  | otherwise = max b (min a number)

-- | Map over the a produced by an animation at every frame.
mapA :: (a -> b) -> Animation a -> Animation b
mapA fn (Animation d f) = Animation d (fn . f)

-- | @takeA duration animation@ creates a new animation consisting of initial segment of
--   @animation@ of given @duration@, played at the same rate as the original animation.
--
--  The @duration@ parameter is clamped to be between 0 and @animation@'s duration.
--  New animation duration is equal to (eventually clamped) @duration@.
takeA :: Duration -> Animation a -> Animation a
takeA len (Animation d gen) = Animation len' $ \t ->
    gen (t * len'/d)
  where
    len' = clamp 0 d len

-- | @dropA duration animation@ creates a new animation by dropping initial segment
--   of length @duration@ from the provided @animation@, played at the same rate as the original animation.
--
--  The @duration@ parameter is clamped to be between 0 and @animation@'s duration.
--  The duration of the resulting animation is duration of provided @animation@ minus (eventually clamped) @duration@.
dropA :: Duration -> Animation a -> Animation a
dropA len (Animation d gen) = Animation len' $ \t ->
    gen (t * len'/d + len/d)
  where
    len' = d - clamp 0 d len

-- | @lastA duration animation@ return the last @duration@ seconds of the animation.
lastA :: Duration -> Animation a -> Animation a
lastA len a = dropA (duration a - len) a

-- | Freeze the last frame for @t@ seconds at the end of the animation.
pauseAtEnd :: Monoid a => Duration -> Animation a -> Animation a
pauseAtEnd t a = a `andThen` pause t

-- | Freeze the first frame for @t@ seconds at the beginning of the animation.
pauseAtBeginning :: Semigroup a => Duration -> Animation a -> Animation a
pauseAtBeginning t a =
    Animation t (freezeFrame 0 a) <> a

-- | Freeze the first and the last frame of the animation for a specified duration.
pauseAround :: Monoid a => Duration -> Duration -> Animation a -> Animation a
pauseAround start end = pauseAtEnd end . pauseAtBeginning start

-- Freeze frame at time @t@.
freezeFrame :: Time -> Animation a -> (Time -> a)
freezeFrame t (Animation d f) = const $ f (t/d)

-- | Change the duration of an animation. Animates are stretched or squished
--   (rather than truncated) to fit the new duration.
adjustDuration :: (Duration -> Duration) -> Animation a -> Animation a
adjustDuration fn (Animation d gen) =
  Animation (fn d) gen

-- | Set the duration of an animation by adjusting its playback rate. The
--   animation is still played from start to finish without being cropped.
setDuration :: Duration -> Animation a -> Animation a
setDuration newD = adjustDuration (const newD)

-- | Modify the time component of an animation. Animation duration is unchanged.
signalA :: Signal -> Animation a -> Animation a
signalA fn (Animation d gen) = Animation d $ gen . fn

-- | Play an animation in reverse. Duration remains unchanged. Shorthand for:
--   @'signalA' 'reverseS'@.
reverseA :: Animation a -> Animation a
reverseA = signalA reverseS

-- | Play animation before playing it again in reverse. Duration is twice
--   the duration of the input.
playThenReverseA :: Semigroup a => Animation a -> Animation a
playThenReverseA a = a <> reverseA a

-- | Loop animation @n@ number of times. This number may be fractional and it
--   may be less than 1. It must be greater than or equal to 0, though.
--   New duration is @n*duration input@.
repeatA :: Double -> Animation a -> Animation a
repeatA n (Animation d f) = Animation (d*n) $ \t ->
  f ((t*n) `mod'` 1)

-- | @freezeAtPercentage time animation@ creates an animation consisting of stationary frame,
-- that would be displayed in the provided @animation@ at given @time@.
-- The duration of the new animation is the same as the duration of provided @animation@.
freezeAtPercentage :: Time        -- ^ value between 0 and 1. The frame displayed at this point in the original animation will be displayed for the duration of the new animation
                   -> Animation a -- ^ original animation, from which the frame will be taken
                   -> Animation a -- ^ new animation consisting of static frame displayed for the duration of the original animation
freezeAtPercentage frac (Animation d genFrame) =
  Animation d $ const $ genFrame frac

-- for testing
--

demoA1, demoA2, demoA :: Animation String
demoA1 = anim (\x -> if x == 0 then ("") else ("A1 @" ++ (show x)))
demoA2 = adjustDuration (*2) $ anim (("A2 @" ++) . show)
demoA = demoA1 <> demoA2

sampleFew :: Show b => Animation b -> IO ()
sampleFew a = mapM_ (putStrLn . show) $ zip ts $ map (\t -> frameAt t a) ts
  where
    ts = [0, 0.5, 0.9, 1, 1.1, 1.5, 2, 2.5, 3]
