module Monster
    (   Lived (..),
        Bag (..),
        Named (..),
        Enemy (..),
        EnemyType (..),
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

data EnemyType = Zombie | Skeleton | Goblin | Wisp | Chicken

data Enemy = Enemy {
    enemyType :: EnemyType,
    enemyHp :: Int,
    enemyDamage :: Int,
    enemyGold :: Int
}

enemyName t = case t of
    Zombie -> "zombie"
    Skeleton -> "skeleton"
    Goblin -> "goblin"
    Wisp -> "wisp"
    Chicken -> "chicken"

instance Lived Enemy where
    viewHp = enemyHp
    viewDamage = enemyDamage
    updateHp e f = e { enemyHp = f $ enemyHp e }

instance Bag Enemy where
    viewCoin = enemyGold
    earnCoin e _ = e
    useCoin e _ = e

instance Named Enemy where
    viewName = enemyName . enemyType