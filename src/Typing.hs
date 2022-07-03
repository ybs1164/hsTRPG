module Typing 
    ( typingString
    ) where

import Control.Concurrent
import System.IO


typingString [] = return True
typingString (x:xs) = do
    putChar x
    threadDelay 20000
    typingString xs