-- | This module defines triangular norm which dual to triangular conorm and it's instances.
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}
module Data.Tnorm 
       ( Tnorm(..), (/*\)
       -- * Monoid
       , Monoid
       -- * Instances
       , Min(..), Product(..), Lukasiewicz(..), Drastic(..)
       , NilpotentMin(..), HamacherProd(..)
       ) where 

import Data.Monoid (Monoid (..))
-- | Generalization of intersection in a lattice and conjunction in logic.
--   Laws:
--     * associativity: (a `tnorm` b) `tnorm` c = a `tnorm` (b `tnorm` c)
-- 
--     * commutativity: a `tnorm` b = b `tnorm` a
-- 
--     * monotonicity:  a `tnorm` b <= c `tnorm` d 
--                         if a <= c and b <= d
--     
--     * identity:      1 `tnorm` a = a = a `tnorm` 1
-- 
--     * annihilation:  0 `tnorm` a = 0 = a `tnorm` 0
--
--   Since Tnorm function are numeric and Tnorn are partially ordered we forced Ord and Num contraits.
class (Ord a, Num a) => Tnorm a where 
  tnorm :: a -> a -> a
  tnorm = min

-- Since tnorm forms a monoid.
instance Tnorm a => Monoid a where
  mempty  = 1
  mappend = tnorm

-- | Alias to 'tnorm'. Name choosed this way to not shade 'Control.Applicative.(<*>)'
{-# INLINE (/*\) #-}
(/*\) :: Tnorm a => a -> a -> a
(/*\) = tnorm

-- | Dual to 'Max'. Minimum t-norm, also called the Godel t-norm. Represent standart semantic for weak conjunction. It's the pointwise largest possible t-norm.
newtype Min a = Min { getMin :: a }
                deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)

instance (Ord a, Num a) => Tnorm (Min a) where
  tnorm = min

-- | Dual to 'ProbSum'. The product t-norm is standard semantics for strong 
newtype Product a = Product { getProduct :: a }
                    deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
  
instance (Ord a, Num a) => Tnorm (Product a) where
  tnorm = (*)

-- | Dual to 'BoundedSum'. Standart semantics for strng conjunction in Lukasiewicz fuzzy logic.  
newtype Lukasiewicz a = Lukasiewicz { getLukasiewicz :: a }
                        deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)

instance (Ord a, Num a) => Tnorm (Lukasiewicz a) where
  tnorm a b = max 0 (a + b - 1)

-- | Dual to 'Codrastic'. Pointwise smallest t-norm.
newtype Drastic a = Drastic { getDrastic :: a }
                    deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                             
instance (Eq a, Ord a, Num a) => Tnorm (Drastic a) where
  tnorm p q |   p == 1   = q
            |   q == 1   = p
            | otherwise = 0

-- | Dual to 'NilpotentMax'
newtype NilpotentMin a = NilpotentMin { getNilpotentMin :: a }
                    deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)
                             
instance (Ord a, Num a) => Tnorm (NilpotentMin a) where
  tnorm p q | p + q > 1 = min p q
            | otherwise = 0

-- | Dual to 'EinsteinSum'. Strict Archimedean t-norm.
newtype HamacherProd a = HamacherProd { getHamacherProd :: a }
                         deriving (Show, Read, Eq, Ord, Num, Fractional, Floating)


instance (Eq a, Ord a, Num a, Fractional a) => Tnorm (HamacherProd a) where
  tnorm p q | p == q && p == 0 = 0
            |   otherwise   = (p * q) / (p + q - p * q)
