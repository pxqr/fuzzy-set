-- | This module defines triangular conorm which dual to triangular norm and it's instances.
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Tconorm 
       ( Tconorm(snorm)
       -- * Type mappings
       , DualTnorm, DualTconorm  
       -- * Dual
       , Dual(..)
       -- * Instances
       , Max(..), ProbSum(..), BoundedSum(..), Codrastic(..)
       , NilpotentMax(..), EinsteinSum(..)
       ) where

import Data.Tnorm 

-- | T-conorms are dual to t-norms under order-reversing operation and used to represent logical disjunction in fuzzy logic and union in fuzzy set theory. Laws:
-- 
--     * identity elem: snorm a 0 = a = snorm 0 a
--     
--     * annihilation:  snorm a 1 = 1 = snorm 1 a    
-- 
--     * commutativity: a `snorm` b = b `snorm` a
-- 
--     * associativity: (a `snorm` b) `snorm` c = a `snorm` (b `snorm` c)
-- 
--     * monotonicity:  snorm a b <= snorm c d if a <= c and b <= d
class (Ord a, Num a) => Tconorm a where
  snorm :: a -> a -> a
  snorm = max

-- | Type mapping from t-conorm to t-norm
type family DualTnorm (tconorm :: * -> *) :: * -> *
type instance DualTnorm Max          = Min
type instance DualTnorm ProbSum      = Product
type instance DualTnorm BoundedSum   = Lukasiewicz
type instance DualTnorm Codrastic    = Drastic
type instance DualTnorm NilpotentMax = NilpotentMin
type instance DualTnorm EinsteinSum  = HamacherProd

-- | Type mapping from t-norm to t-conorm. Inverse of DualTnorm
type family DualTconorm (tnorm :: * -> *) :: * -> *
type instance DualTconorm Min          = Max
type instance DualTconorm Product      = ProbSum
type instance DualTconorm Lukasiewicz  = BoundedSum
type instance DualTconorm Drastic      = Codrastic
type instance DualTconorm NilpotentMin = NilpotentMax
type instance DualTconorm HamacherProd = EinsteinSum

-- | Since t-conorm is dual to t-norm we can define t-conorm complementary from generalizes De Morgan's law's.
newtype Dual a = Dual { getDual :: a }
               deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                         
instance Tnorm a => Tconorm (Dual a) where
  snorm (Dual p) (Dual q) = Dual (1 - tnorm (1 - p) (1 - q))

-- | Dual to 'Min'. /Max/ is smallest t-conorm.
newtype Max a = Max { getMax :: a }
                deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)

instance (Ord a, Num a) => Tconorm (Max a) where
  snorm = max
  
-- | Dual to 'Product'. In probability theory it expresses the probability of the union of independent events.
newtype ProbSum a = ProbSum { getProbSum :: a } 
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