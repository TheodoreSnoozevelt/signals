module Test where
import Synthesis
import Adsr 
import Data.Fixed

freqInput :: [Float]
freqInput =
  forSeconds 0.2 440 44100 ++
  forSeconds 0.5 880 44100 ++
  forSeconds 0.1 660 44100 ++
  forSeconds 0.3 550 44100

adsrInput :: [AdsrInput]
adsrInput = forSeconds 0.8 Open 44100 ++ forSeconds 0.2 Closed 44100

cubeX :: (Real a, Floating a) => a -> a
cubeX t =
  let phase = mod' t (2*pi)
      intermediate
        | phase < pi/2 = phase * 2 / pi
        | phase < pi = 0
        | phase < pi+pi/2 =(phase - pi) * 2 / pi
        | otherwise = 1
   in intermediate - 0.5

cubeY t =
  let phase = mod' t (2*pi)
      intermediate
        | phase < pi/2 = 0
        | phase < pi = (phase-pi/2) * 2 / pi
        | phase < pi+pi/2 = 1
        | otherwise = (phase - (pi+pi/2)) * 2 / pi
  in intermediate - 0.5

rotate x y theta = [x * cos theta - y * sin theta, x * sin theta + y * cos theta]

experiment1 :: [[Float]]
experiment1 = 
  let freq = forSeconds 5 440 44100
      x = runSynth cubeX freq
      y = runSynth cubeY freq
      zipped = zip x y
      thing = map (\(a,b) -> [a,b]) zipped
      thetas = map (/ 10000) [1..44100*5]
      ok = zip thing thetas
  in [rotate a b r | ([a,b], r) <- ok]
  

experiment2 :: [[Float]]
experiment2 = 
   let fm = runSynth sin (forSeconds 5 1 44100)
       zipper mod inp = inp * (1 + mod * 0.05)
       frequencies = zipWith zipper fm (concat $ repeat freqInput)
       base = runSynth sin (forSeconds 5 220 44100)
       melody = runSynth saw frequencies
       mixed = mixFixed 0.5 base melody
       envelopeGenerator = Adsr 0.05 0.05 0.7 0.2
       envelope = runAdsr 44100 envelopeGenerator (concat $ repeat adsrInput)
       in toStereo $ zipWith (*) mixed envelope
       