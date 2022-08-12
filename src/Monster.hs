module Monster
    (   Lived (..),
        Bag (..),
        Named (..),
        HasStatus (..),
        Enemy (..),
        EnemyType (..),
        Player (..),
        Status (..),
    ) where

class Lived a where
    viewHp :: a -> Int
    viewDamage :: a -> Int
    updateHp :: a -> (Int -> Int) -> a
    revive :: a -> a

class Named a where
    viewName :: a -> String

class Bag a where
    viewCoin :: a -> Int
    earnCoin :: a -> Int -> a
    useCoin :: a -> Int -> a

class HasStatus a where
    viewStatus :: a -> (Int -> Status) -> Int
    addStatus :: a -> Status -> a


-- Thorn -> receive damage when attacked
-- Fire -> damaged when other's turn end
-- Drain -> heal when attack
-- Shield -> decrease damage n attack
-- Stun -> dont active one turn


data Status = 
    Fire Int 
    | Undead Int
    | Shield Int

-- compare status type
statusKindComp :: Status -> Status -> Bool
statusKindComp (Fire _) (Fire _) = True
statusKindComp (Undead _) (Undead _) = True
statusKindComp _ _ = False

statusValue :: Status -> Int
statusValue (Fire x) = x
statusValue (Undead x) = x
statusValue (Shield x) = x


data Player = Player {
    playerName :: String,
    playerMaxHp :: Int,
    playerHp :: Int,
    playerDamage :: Int,
    playerGold :: Int,
    playerStatus :: [Status]
}

instance Lived Player where
    viewHp = playerHp
    viewDamage = playerDamage
    updateHp p f = p { playerHp = f $ playerHp p }
    revive p = p { playerHp = playerMaxHp p }

instance Bag Player where
    viewCoin = playerGold
    earnCoin p earned = p { playerGold = playerGold p + earned }
    useCoin p used = p { playerGold = playerGold p - used }

instance HasStatus Player where
    viewStatus p s = foldr ((+) . statusValue) 0 (filter (statusKindComp $ s 0) $ playerStatus p)
    addStatus p s = p { playerStatus = s : playerStatus p }

data EnemyType = Zombie | Skeleton | Goblin | Wisp | Chicken

data Enemy = Enemy {
    enemyType :: EnemyType,
    enemyMaxHp :: Int,
    enemyHp :: Int,
    enemyDamage :: Int,
    enemyGold :: Int,
    enemyStatus :: [Status]
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
    revive e = e { enemyHp = enemyMaxHp e }

instance Bag Enemy where
    viewCoin = enemyGold
    earnCoin e _ = e
    useCoin e _ = e

instance Named Enemy where
    viewName = enemyName . enemyType

instance HasStatus Enemy where
    viewStatus e s = foldr ((+) . statusValue) 0 (filter (statusKindComp $ s 0) $ enemyStatus e)
    addStatus e s = e { enemyStatus = s : enemyStatus e }