module Main where

import Lib
import Typing
import Monster
import Control.Concurrent
import System.IO

startingSenario = "Hello, there. What's your name? "
welcomeSenario name = "Hello, " ++ name ++ ". Now you just have wonderful adventure in this program.\n"
welcomeSenario_ = "Are you ready? Let's go!\n"

data Event = Encounter Enemy | GetItem | GetGold Int | Shop | Idle
data Action = Attack | UseItem | Go

getEvent :: IO Event
getEvent = do
    rand <- randomFunc
    let index = rand `mod` 3
    case index of
        0 -> do
            rand <- randomFunc
            let gold = 5 + rand `mod` 6
            return $ GetGold gold
        1 -> return Idle
        2 -> do
            rand <- randomFunc
            let index = rand `mod` 5
            let monster = case index of
                    0 -> Goblin 10 3
                    1 -> Zombie 20 1
                    2 -> Wisp 5 1
                    3 -> Skeleton 1 5
                    4 -> Chicken 8 0 
                    _ -> Goblin 1 1
            return Encounter monster
        _ -> return GetItem

playerIdle :: Player -> IO ()
playerIdle player = do
    threadDelay 500000
    event <- getEvent
    case event of
        GetGold g -> do
            typingString "Lucky! You pick up some golds!\n"
            threadDelay 500000
            typingString $
                "You earn " ++ show g ++ " golds. Now you have " ++ show (coin player + g) ++ " golds.\n"
            playerIdle (earnCoin player g)
        Idle -> do
            typingString "You just walk around.\n"
            playerIdle player
        _ -> do
            typingString "Something wrong\n"
            return ()

main :: IO ()
main = do
    typingString startingSenario
    playerName <- getLine
    typingString $ welcomeSenario playerName
    threadDelay 1000000
    typingString welcomeSenario_
    playerIdle (Player 20 10 5)