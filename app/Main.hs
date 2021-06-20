module Main where

import Wave
import Adsr
import Synthesis
import qualified Data.ByteString as B
import Control.Monad.State

freqInput :: [Float]
freqInput =
  forSeconds 0.2 440 44100 ++
  forSeconds 0.5 880 44100 ++
  forSeconds 0.1 220 44100 ++
  forSeconds 0.3 110 44100

adsrInput :: [AdsrInput]
adsrInput = forSeconds 0.8 Open 44100 ++ forSeconds 0.2 Closed 44100


main :: IO ()
main = do
   let base = runSynth sin (forSeconds 1.1 220 44100)
   let melody = runSynth saw freqInput
   let mixed = mixFixed 0.5 base melody
   let envelopeGenerator = Adsr 0.05 0.05 0.7 0.2
   let envelope = runAdsr 44100 envelopeGenerator adsrInput
   let result = zipWith (*) mixed envelope
   let wavFile = createWavBinary $ WavFile 44100 2 (toStereo result)
   B.writeFile "test.wav" wavFile
