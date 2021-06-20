module Main where

import Wave
import Synthesis
import qualified Data.ByteString as B
import Control.Monad.State

freqInput :: [Float]
freqInput = concat [
  forSeconds 0.2 440 44100,
  forSeconds 0.5 880 44100,
  forSeconds 0.1 220 44100,
  forSeconds 0.3 110 44100]

main :: IO ()
main = do
   let base = runSynth sin (forSeconds 1.1 220 44100)
   let melody = runSynth saw freqInput
   let mixed = mixFixed 0.5 base melody
   let wavFile = createWavBinary $ WavFile 44100 2 (toStereo mixed)
   B.writeFile "test.wav" wavFile
