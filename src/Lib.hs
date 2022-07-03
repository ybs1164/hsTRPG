module Lib
    ( randomFunc,
      len,
      randomListValue
    ) where

import System.Random

randomFunc :: IO Int
randomFunc = randomIO

len l = fromIntegral (length l)

randomListValue :: [a] -> IO a
randomListValue l = do 
  rand <- randomFunc
  let index = mod rand (len l)
  return $ l !! index