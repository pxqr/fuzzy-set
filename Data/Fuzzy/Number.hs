module Data.Fuzzy.Number where

import Data.Fuzzy.Mu

type Number = Mu Double Double

--data Number = Number { number :: Mu Double Double  }

-- | p >= 0
slopee :: Floating a => a -> a -> a
slopee p x = exp $ negate $ abs x ** p

-- | p >= 0
sloper :: Floating a => a -> a -> a
sloper p x = 1 / (1 + abs x ** p)


-- | Fuzzy numbers as mapping. Useful to define.
lr :: (Ord a, Fractional a) => (a -> a) -> (a -> a) -> a -> a -> a -> Mu a a
lr l r alpha moda beta = Mu f
  where f x | x <= moda = l ((moda - x) / alpha)
            | otherwise = r ((x - moda) / beta)


lrs :: (Ord a, Floating a) => a -> a -> a -> Mu a a
lrs = sloper 1 `lr` sloper 1

num :: (Ord a, Floating a) => a -> Mu a a
num x = (sloper 5 `lr` sloper 5) 0.5 x 0.5
