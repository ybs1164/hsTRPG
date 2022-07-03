module Main where

import Lib
import Typing
import Monster (
    Lived (..),
    Bag (..),
    Named (..),
    Enemy (..),
    Player (..))
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
            let t = rand `mod` 5
            let monster = case t of
                    0 -> Enemy "skeleton" 2 1
                    1 -> Enemy "zombie" 1 2
                    2 -> Enemy "goblin" 3 2
                    3 -> Enemy "wisp" 1 1
                    _ -> Enemy "chicken" 1 0
            return $ Encounter monster
        _ -> return GetItem

fight :: Player -> Enemy -> IO Player
fight player enemy = do
    typingString ("Player Life : " ++ show (viewHp player) ++ "\n"
        ++ toUpperFirst (viewName enemy) ++ " Life : " ++ show (viewHp enemy) ++ "\n")
    typingString "Select what to do : 1. attack > "
    todo <- getLine
    case todo of
        "attack" -> do
            let dmg = viewDamage player
            let damagedEnemy = updateHp enemy (\h -> h - dmg)
            let damagedPlayer = updateHp player (\h -> h - viewDamage enemy) 
            typingString ("You attack enemy " ++ show dmg ++ " damage.\n")
            if viewHp damagedEnemy < 0 then do
                typingString "You win!\n"
                return player
            else do
                typingString ("You attacked by enemy " ++ show (viewDamage enemy) ++ " damage.\n")
                fight damagedPlayer damagedEnemy
        _ -> do
            typingString "Not exists this command. try again.\n"
            fight player enemy

playerIdle :: Player -> IO ()
playerIdle player = do
    event <- getEvent
    case event of
        GetGold g -> do
            typingString "Lucky! You pick up some golds!\n"
            typingString $
                "You earn " ++ show g ++ " golds. Now you have " ++ show (viewCoin player + g) ++ " golds.\n"
            playerIdle (earnCoin player g)
        Idle -> do
            typingString "You just walk around.\n"
            playerIdle player
        Encounter enemy -> do
            typingString (toUpperFirst $ viewName enemy ++ " encountered your front!\n")
            fightedPlayer <- fight player enemy
            playerIdle fightedPlayer
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
    playerIdle (Player playerName 20 10 5)