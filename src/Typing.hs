module Typing 
    ( typingString,
      toUpperFirst
    ) where

import Control.Concurrent
import System.IO
import Data.Char


typingString [] = do
    threadDelay 500000
    return True
typingString (x:xs) = do
    putChar x
    threadDelay 20000
    typingString xs

toUpperFirst [] = []
toUpperFirst (x:xs) = toUpper x:xs