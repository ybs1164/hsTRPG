module Lib
    ( randomFunc,
      len,
      randomRange,
      randomListValue
    ) where

import System.Random
import System.Random.Stateful (
  uniformRM,
  globalStdGen )

randomFunc :: IO Int
randomFunc = randomIO

randomRange :: Int -> Int -> IO Int
randomRange f l = uniformRM (f, l) globalStdGen

len l = fromIntegral (length l)

randomListValue :: [a] -> IO a
randomListValue l = do 
  rand <- randomFunc
  let index = mod rand (len l)
  return $ l !! index