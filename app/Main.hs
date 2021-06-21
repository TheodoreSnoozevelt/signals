module Main where

import Wave
import Adsr
import Synthesis
import qualified Data.ByteString as B
import Data.Fixed
import Test

data Pos = Pos Double Double deriving (Show, Read)

scale :: Pos -> [Double]
scale pos =
  let Pos x y = pos
  in [x / 2000, y / 2000]

nPos :: [[Double]] -> Float -> Int -> Float
nPos points t i =
  let len = length points
      idx = mod' t 2/pi
      index = round $ (idx / 2*pi) * (fromIntegral len - 1)
  in realToFrac ((points !! index) !! i) - 0.5

xPos :: [[Double]] -> Float -> Float
xPos points t = nPos points t 0

yPos :: [[Double]] -> Float -> Float
yPos points t = nPos points t 1


main :: IO()
main = do
  content <- readFile "test.points"
  let result = read content
  let scaled = map scale result
  let freq = forSeconds 1 55 44100 ++ forSeconds 1 75 44100
  let x = runSynth (xPos scaled) freq
  let y = map (*(-1)) $ runSynth (yPos scaled) freq
  let zipped = zip x y
  let res = map (\(a,b) -> [a,b]) zipped
  let wavFile = createWavBinary $ WavFile 44100 2 res
  B.writeFile "test.wav" wavFile

