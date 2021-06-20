module Wave where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as W

data WavFile = WavFile { wavFileSampleRate :: Int, wavFileChannels :: Int, wavFileSamples :: [[Float]] }

toBytes :: String -> [W.Word8]
toBytes = map (toEnum . fromEnum)

createWavHeader :: WavFile -> B.ByteString
createWavHeader wavFile = BL.toStrict $ Builder.toLazyByteString header 
  where 
    nSamples = length $ wavFileSamples wavFile
    nChannels = wavFileChannels wavFile 
    riffWords = Builder.string7 "RIFF"
    size = Builder.word32LE $ fromIntegral (36 + nSamples * nChannels * 2)
    wavId = Builder.string7  "WAVE"
    fmtId = Builder.string7  "fmt "
    fmtSize = Builder.word32LE 16
    fmtTag = Builder.word16LE 1
    iSampleRate = wavFileSampleRate wavFile
    channels = Builder.word16LE $ fromIntegral nChannels
    sampleRate = Builder.word32LE $ fromIntegral iSampleRate
    bytePerSec = Builder.word32LE $ fromIntegral $ iSampleRate * nChannels * 2
    blockSize = Builder.word16LE $ fromIntegral $ nChannels * 2
    bit = Builder.word16LE 16
    dataId = Builder.string7 "data"
    dataSize = Builder.word32LE $ fromIntegral $ nSamples * nChannels * 2
    header = mconcat [riffWords, size, wavId, fmtId, fmtSize, fmtTag, channels, sampleRate, bytePerSec, blockSize, bit, dataId, dataSize]    

createWavBinary :: WavFile -> B.ByteString
createWavBinary wavFile = B.concat [createWavHeader wavFile, BL.toStrict $ Builder.toLazyByteString frames]
  where
    factor :: Float
    factor = 2 ** 14
    flattened = concat $ wavFileSamples wavFile
    integral = map (fromInteger . round . (* factor)) flattened
    frames = mconcat $ map Builder.int16LE integral
