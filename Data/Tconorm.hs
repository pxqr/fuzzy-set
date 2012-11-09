-- | This module defines triangular conorm which dual to triangular norm and it's instances.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Tconorm 
       ( Tconorm(snorm)
       -- * Dual
       , Dual(..)
       -- * Instances
       , Max(..), ProbSum(..), BoundedSum(..), Codrastic(..)
       , NilpotentMax(..), EinsteinSum(..)
       ) where

import Data.Tnorm (Tnorm(tnorm))

-- | T-conorms are dual to t-norms under order-reversing operation and used to represent logical disjunction in fuzzy logic and union in fuzzy set theory.
-- 
--     * identity elem: snorm a 0 = a = snorm 0 a
--
--     * commutativity: a `snorm` b = b `snorm` a
--
--     * associativity: (a `snorm` b) `snorm` c = a `snorm` (b `snorm` c)
-- 
--     * monotonicity:  snorm a b <= snorm c d if a <= c and b <= d
class (Ord a, Num a) => Tconorm a where
  snorm :: a -> a -> a
  snorm = max

-- | Since t-conorm is dual to t-norm we can define t-conorm complementary from generalizes De Morgan's law's.
newtype Dual a = Dual { getDual :: a }
               deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                         
instance Tnorm a => Tconorm (Dual a) where
  snorm (Dual p) (Dual q) = Dual (1 - tnorm (1 - p) (1 - q))

-- | Dual to 'Min'.
newtype Max a = Max { getMax :: a }
                deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)

instance (Ord a, Num a) => Tconorm (Max a) where
  snorm = max
  
-- | Dual to 'Product'.
newtype ProbSum a = Sum { getSum :: a } 
                deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                         
instance (Ord a, Num a) => Tconorm (ProbSum a) where
  snorm p q = p + q - p * q
  
-- | Dual to the Lukasiewicz.
newtype BoundedSum a = BoundedSum { getBoundedSum :: a }
                deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                         
instance (Ord a, Num a) => Tconorm (BoundedSum a) where
  snorm p q = min (p + q) 1
  
-- | Dual to the 'Drastic'.
newtype Codrastic a = Codrastic { getCodrastic :: a }
                    deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)

instance (Ord a, Num a) => Tconorm (Codrastic a) where
  snorm p q |   p == 0   = q
            |   q == 0   = p
            | otherwise = 1

-- | Dual to the 'NilpotentMin'.
newtype NilpotentMax a = NilpotentMax { getNilpotentMax :: a }
                       deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                             
instance (Ord a, Num a) => Tconorm (NilpotentMax a) where
  snorm p q | p + q < 1 = max p q
            | otherwise = 1

-- | Dual to the 'HamacherProd'.
newtype EinsteinSum a = EinsteinSum { getEinsteinSum :: a }
                      deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)

instance (Ord a, Num a, Fractional a) => Tconorm (EinsteinSum a) where
  snorm p q = (p + q) / (1 + p * q)