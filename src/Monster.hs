module Monster
    (   Lived (..),
        Bag (..),
        Named (..),
        Enemy (..),
        Player (..)
    ) where

class Lived a where
    viewHp :: a -> Int
    viewDamage :: a -> Int
    updateHp :: a -> (Int -> Int) -> a

class Named a where
    viewName :: a -> String

class Bag a where
    viewCoin :: a -> Int
    earnCoin :: a -> Int -> a
    useCoin :: a -> Int -> a


-- Thorn -> receive damage when attacked
-- Fire -> damaged when other's turn end
-- Drain -> heal when attack
-- Shield -> decrease damage n attack
-- Stun -> dont active one turn
-- Revive -> once


data Player = Player {
    playerName :: String,
    playerHp :: Int,
    playerDamage :: Int,
    playerGold :: Int
}

instance Lived Player where
    viewHp = playerHp
    viewDamage = playerDamage
    updateHp p f = p { playerHp = f $ playerHp p }

instance Bag Player where
    viewCoin = playerGold
    earnCoin p earned = p { playerGold = playerGold p + earned }
    useCoin p used = p { playerGold = playerGold p - used}

data Enemy = Enemy {
    enemyName :: String,
    enemyHp :: Int,
    enemyDamage :: Int
}

instance Lived Enemy where
    viewHp = enemyHp
    viewDamage = enemyDamage
    updateHp e f = e { enemyHp = f $ enemyHp e }

instance Named Enemy where
    viewName = enemyName