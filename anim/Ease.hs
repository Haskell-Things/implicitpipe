{-|
  Easing functions modify the rate of change in animations.
  More examples can be seen here: <https://easings.net/>.
-}
module Ease
  ( Signal
  , constantS
  , fromToS
  , reverseS
  , curveS
  , powerS
  , bellS
  , oscillateS
  , cubicBezierS
  ) where

-- | Signals are time-varying variables. Signals can be composed using function
--   composition.
type Signal = Double -> Double

-- | Constant signal.
constantS :: Double -> Signal
constantS = const

-- | Signal with new starting and end values.
fromToS :: Double -> Double -> Signal
fromToS from to t = from + (to-from)*t

-- | Reverse signal order.
reverseS :: Signal
reverseS t = 1-t

-- | S-curve signal. Takes a steepness parameter. 2 is a good default.
curveS :: Double -> Signal
curveS steepness s =
  if s < 0.5
    then 0.5 * (2*s)**steepness
    else 1-0.5 * (2 - 2*s)**steepness

-- | Power curve signal. Takes a steepness parameter. 2 is a good default.
powerS :: Double -> Signal
powerS steepness s = s**steepness

-- | Oscillate signal.
oscillateS :: Signal
oscillateS t =
  if t < 1/2
    then t*2
    else 2-t*2

-- | Bell-curve signal. Takes a steepness parameter. 2 is a good default.
bellS :: Double -> Signal
bellS steepness = curveS steepness . oscillateS

-- | Cubic Bezier signal. Gives you a fair amount of control over how the
--   signal will curve.
cubicBezierS :: (Double, Double, Double, Double) -> Signal
cubicBezierS (x1, x2, x3, x4) s =
  let ms = 1-s
  in x1*ms^(3::Int) + 3*x2*ms^(2::Int)*s + 3*x3*ms*s^(2::Int) + x4*s^(3::Int)

