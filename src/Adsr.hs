module Adsr where

import Control.Monad.State 
  
data AdsrInput = Open | Closed  
data AdsrState = Idle | Attack | Decay | Sustain | Release
data Adsr = Adsr Float Float Float Float

data AdsrInternal = AdsrInternal { adsrValues :: Adsr, adsrState :: AdsrState, timeInState :: Float, lastInput :: AdsrInput, stateStartingValue :: Float, currentValue :: Float }

adsrStep :: Float -> AdsrInput -> AdsrInternal -> AdsrInternal
adsrStep sampleRate input internal =
  let Adsr a d s r = adsrValues internal 
      currentState = adsrState internal
      timeStep = 1 / sampleRate
      currentTime = timeInState internal
      newTime = currentTime + timeStep
      (newState, computedTime, newStarting) = case currentState of 
        Idle -> case input of 
          Open -> (Attack, 0, 0)
          Closed -> (Idle, 0, 0)
        Attack -> case input of
          Open -> if currentTime < a then (Attack, newTime, 0) else (Decay, 0, 0)
          Closed -> (Release, 0, 0)
        Decay -> case input of 
          Open -> if currentTime < d then (Decay, newTime, 0) else (Sustain, 0, 0)
          Closed -> (Release, 0, 0)
        Sustain -> case input of
          Open -> (Sustain, newTime, 0)
          Closed -> (Release, 0, 0)
        Release -> if currentTime < r then (Release, newTime, 0) else (Idle, 0, 0)
  in internal { timeInState = computedTime, adsrState = newState, lastInput = input, stateStartingValue = newStarting}  

adsrValue :: AdsrInternal -> Float
adsrValue internal =
  let Adsr a d s r = adsrValues internal
      currentState = adsrState internal
      currentTime = timeInState internal
  in case currentState of 
    Idle -> 0
    Attack -> currentTime / a
    Decay -> 1 - (currentTime / d) * (1 - s)
    Sustain -> s 
    Release -> s - s * (currentTime / r)

adsr :: Float -> AdsrInput -> State AdsrInternal Float 
adsr sr inp = do 
  s <- get 
  let newInt = adsrStep sr inp s 
  put newInt 
  return $ adsrValue newInt
  
runAdsr :: Float -> Adsr -> [AdsrInput] -> [Float]
runAdsr sr spec input =
  let internal = AdsrInternal spec Idle 0 Closed 0 0
      runner = forM input (adsr sr)  
  in evalState runner internal 
 




