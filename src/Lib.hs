module Lib
    ( len,
      randomRange,
      randomListValue
    ) where

import System.Random
import System.Random.Stateful (
  uniformRM,
  globalStdGen )

randomRange :: Int -> Int -> IO Int
randomRange f l = uniformRM (f, l) globalStdGen

len l = fromIntegral (length l)

randomListValue :: [a] -> IO a
randomListValue l = do 
  rand <- uniformRM (0, len l) globalStdGen
  return $ l !! rand