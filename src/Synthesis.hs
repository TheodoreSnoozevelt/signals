module Synthesis where

import Control.Monad.State
import Data.Fixed

data OscState = OscState { oscPhase :: Float, oscValue :: Float }

runOsc :: (Float -> Float) -> Float -> State OscState Float
runOsc osc freq = do
  s <- get
  let curPhase = oscPhase s
  let newPhase = curPhase + (freq / 44100)
  let newValue = osc (pi * 2 * newPhase)
  let new = s { oscValue = newValue, oscPhase = newPhase }
  put new
  return $ oscValue s

toList :: a -> a -> [a]
toList x y = [x, y]

toStereo :: [a] -> [[a]]
toStereo items = zipWith toList items items

forSeconds :: Float -> Float -> Float -> [Float]
forSeconds s v sr = replicate times v
  where
    samples = s * sr
    times = round samples

mix1 :: Float -> Float -> Float -> Float
mix1 balance x y = x * balance + y * (1-balance)

mixFixed :: Float -> [Float] -> [Float] -> [Float]
mixFixed m m1 m2 =
  let zipped = zip m1 m2
  in [mix1 m x y | (x,y) <- zipped]

mix :: [Float] -> [Float] -> [Float] -> [Float]
mix balance m1 m2 =
  let zipped = zip3 balance m1 m2
  in [mix1 b x y | (b,x,y) <- zipped]

saw :: Float -> Float
saw x =
  let y = mod' x (2 * pi)
  in (y / (2*pi)) - 0.5

runOscWithInput :: (Float -> Float) -> [Float] -> State OscState [Float]
runOscWithInput osc input = forM input (runOsc osc)

runSynth :: (Float -> Float) -> [Float] -> [Float]
runSynth osc input = evalState (runOscWithInput osc input) (OscState 0 0)

