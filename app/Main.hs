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

-- data Action = Attack | UseItem | Go | Run


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
        
printInfoList :: [String] -> IO Bool
printInfoList [] = return True
printInfoList (info:infoList) = do
    typingString info
    printInfoList infoList


fight :: Player -> Enemy -> IO Player
fight player enemy = do
    let infoList = [infoLife]
    let infoList = "Select what to do : 1. attack, 2. run > ":infoList
    printInfoList infoList
    todo <- getLine
    let (printContent, loopCheck, nextedPlayer, nextedEnemy) = action todo
    printInfoList printContent
    if loopCheck then do
        fight nextedPlayer nextedEnemy
    else do
        return nextedPlayer
    -- case todo of
    --     "1" -> attack
    --     "2" -> do
    --         typingString "Run away.\n"
    --         return player
    --     _ -> do
    --         typingString "Not exists this command. try again.\n"
    --         fight player enemy
    where
        infoLife = "Player Life : " ++ show (viewHp player) ++ "\n"
            ++ toUpperFirst (viewName enemy) ++ " Life : " ++ show (viewHp enemy) ++ "\n"
        attack = do
            let (damagedPlayer, damagedEnemy) = getAttackedPlayerEnemy player enemy
            typingString ("You attack enemy " ++ show (viewDamage player) ++ " damage.\n")
            checkWin damagedPlayer damagedEnemy

        getAttackedPlayerEnemy :: Player -> Enemy -> (Player, Enemy)
        getAttackedPlayerEnemy p e = (updateHp p (subtract $ viewDamage e), updateHp e (subtract $ viewDamage p))

        checkWin p e = if viewHp e < 0 then do
                if viewStatus e Undead > 0 then do
                    typingString "Enemy dead, and revive.\n"
                    fight player $ revive (addStatus e (Undead (-1)))
                else do
                    typingString "You win!\n"
                    let g = enemyGold enemy
                    typingString $
                        "You earn " ++ show g ++ " golds. Now you have " ++ show (viewCoin player + g) ++ " golds.\n"
                    return $ earnCoin player g
            else do
                typingString ("You attacked by enemy " ++ show (viewDamage enemy) ++ " damage.\n")
                fight p e
        
        action :: String -> ([String], Bool, Player, Enemy)
        action select = case select of
            "1" -> ([], True, player, enemy)
            "2" -> ([], True, player, enemy)
            _ -> ([], False, player, enemy)

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