module Monster
    (   Lived (..),
        Bag (..),
        Named (..),
        Enemy (..),
        EnemyType (..),
        Player (..),
        Status (..),
        enableStatus,
        viewStatus,
        addStatus
    ) where
import Data.Monoid
import Data.Bits (Bits(xor))

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

-- Thorn -> receive damage when attacked
-- Fire -> damaged when other's turn end
-- Drain -> heal when attack
-- Shield -> decrease damage n attack
-- Stun -> dont active one turn
-- Revive -> once

data Status = Fire Int | Undead Int

statusKindComp :: Status -> Status -> Bool
statusKindComp (Fire _) (Fire _) = True
statusKindComp (Undead _) (Undead _) = True
statusKindComp _ _ = False

statusValue :: Status -> Int
statusValue (Fire x) = x
statusValue (Undead x) = x

enableStatus :: [Status] -> (Int -> Status) -> Bool
enableStatus statusList status = any (statusKindComp $ status 0) statusList

viewStatus :: [Status] -> (Int -> Status) -> Int
viewStatus statusList status = foldr ((+) . statusValue) 0 (filter (statusKindComp $ status 0) statusList)

addStatus :: [Status] -> Status -> [Status]
addStatus statusList s = s: statusList

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
    useCoin p used = p { playerGold = playerGold p - used}

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