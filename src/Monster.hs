module Monster
    (   Lived (..),
        Bag (..),
        Monsters (..),
        Enemy (..),
        Player (..)
    ) where

class Lived a where
    hp :: a -> Int
    damage :: a -> Int
    updateHp :: (Int -> Int) -> a -> a

class Bag a where
    coin :: a -> Int
    earnCoin :: a -> Int -> a
    useCoin :: a -> Int -> a


-- Thorn -> receive damage when attacked
-- Fire -> damaged when other's turn end
-- Drain -> heal when attack
-- Shield -> decrease damage n attack
-- Stun -> dont active one turn
-- Revive -> once


data Player = Player Int Int Int

data Monsters = Goblin
              | Wisp
              | Zombie
              | Skeleton
              | Chicken

data Enemy = Enemy Monsters Int Int

instance Lived Player where
    hp (Player hp _ _) = hp
    damage (Player _ _ damage) = damage
    updateHp f (Player hp gold damage) = Player (f hp) gold damage

instance Bag Player where
    coin (Player _ c _) = c
    earnCoin (Player hp coin damage) earned = Player hp (coin + earned) damage
    useCoin (Player hp coin damage) used = Player hp (coin + used) damage

instance Lived Enemy where
    hp (Enemy _ hp _) = hp
    damage (Enemy _ _ d) = d 
    updateHp f (Enemy t hp d) = Enemy t (f hp) d