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
    index <- randomRange 0 2
    case index of
        0 -> do
            gold <- randomRange 5 11
            return $ GetGold gold
        1 -> return Idle
        2 -> do
            t <- randomRange 0 4
            let monster = case t of
                    0 -> Enemy Skeleton 2 2 1 1 [Undead 1] -- undead
                    1 -> Enemy Zombie 1 1 2 1 [Undead 2] -- undead
                    2 -> Enemy Goblin 3 3 2 3 []
                    3 -> Enemy Wisp 1 1 1 0 []
                    _ -> Enemy Chicken 1 1 0 1 []
            return $ Encounter monster
        _ -> return GetItem

fight :: Player -> Enemy -> IO Player
fight player enemy = do
    typingString ("Player Life : " ++ show (viewHp player) ++ "\n"
        ++ toUpperFirst (viewName enemy) ++ " Life : " ++ show (viewHp enemy) ++ "\n")
    typingString "Select what to do : 1. attack, 2. run > "
    todo <- getLine
    case todo of
        "attack" -> do
            let dmgPlayer = viewDamage player
                dmgEnemy = viewDamage enemy 
                damagedEnemy = updateHp enemy (subtract dmgPlayer)
                damagedPlayer = updateHp player (subtract dmgEnemy) 
            typingString ("You attack enemy " ++ show dmgPlayer ++ " damage.\n")
            if viewHp damagedEnemy < 0 then do
                if viewStatus (enemyStatus damagedEnemy) Undead > 0 then do
                    typingString "Enemy dead, and revive.\n"
                    fight player $ revive (damagedEnemy { enemyStatus = addStatus (enemyStatus damagedEnemy) (Undead (-1)) })
                else do
                    typingString "You win!\n"
                    let g = enemyGold enemy
                    typingString $
                        "You earn " ++ show g ++ " golds. Now you have " ++ show (viewCoin player + g) ++ " golds.\n"
                    return $ earnCoin player g
            else do
                typingString ("You attacked by enemy " ++ show dmgEnemy ++ " damage.\n")
                fight damagedPlayer damagedEnemy
        "run" -> do
            typingString "Run away.\n"
            return player
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
    typingString welcomeSenario_
    playerIdle (Player playerName 20 20 10 5 [])