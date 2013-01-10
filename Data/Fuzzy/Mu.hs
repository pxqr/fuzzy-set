{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- TODO: Class Set
-- TODO: Laws
-- TODO: Algebraic operations.
-- TODO: Rename 'universal'.
-- | Characteristic functionno.
module Data.Fuzzy.Mu  {-
       ( Mu(membership)
       -- * Basic operations  
       , union, intersection, complement
       -- * Derived operations
       , difference, disSum
       ) -} where

import Prelude hiding ((.), id)

import Control.Category
import Control.Applicative
import Control.Monad

type FuzzyBool = Double

-- | Characteristic function.
--   Can be:
--     1. Fuzzy set:    Mu a FuzzyBool
--     2. Fuzzy number: Num a => Mu a FuzzyBool
--     3. Infinity(frankly speaking) fuzzy set.
newtype Mu m a = Mu { membership :: m -> a }
                 deriving (Category, Functor, Applicative, Monad)

empty :: Num a => Mu m a
empty = Mu (const 0)

universal :: Num a => Mu m a
universal = Mu (const 1)

member :: Mu m a -> m -> a
member = membership

-- | 'union' with 'empty' and 'complement' form abelian group and also:
--
--   * idempotence:   a `union` a = a
--
union :: Ord b => Mu a b -> Mu a b -> Mu a b
union (Mu f) (Mu g) = Mu $ \ x -> f x `max` g x

infixl 6 `union`

-- | 'intersection' with 'universal' and 'inverse' form abelian group and also:
--
--   * idempotence: a `intersection` a = a
-- 
intersection :: Ord b => Mu a b -> Mu a b -> Mu a b 
intersection (Mu f) (Mu g) = Mu $ \ x -> f x `min` g x

infixl 7 `intersection`

-- | complement
complement :: Num b => Mu a b -> Mu a b
complement = fmap (1 -) 

inverse :: Fractional b => Mu a b -> Mu a b
inverse = fmap (1 /)

-- | Laws
difference :: (Ord b, Num b) => Mu a b -> Mu a b -> Mu a b
difference p q = p `intersection` complement q

infixl 6 `difference`

division :: Mu m a -> Mu m a -> Mu m a
division p q = undefined -- `union`

infixl 7 `division`

-- | Disjunctive sum:
disSum :: (Num b, Ord b) => Mu a b -> Mu a b -> Mu a b
disSum p q = (p `difference` q) `union` (q `difference` p)



instance (Ord a, Num a) => Num (Mu m a) where
  (+) = union
  (*) = intersection
  (-) = difference
  
  negate = complement
  
  abs    = error "Mu.abs: Not Implemented!"
  signum = error "Mu.signum: Not Implemented!"    
    
  fromInteger = Mu . const . fromInteger

instance (Ord a, Fractional a) => Fractional (Mu m a) where
--  (/) :: a -> a -> a
  (/) = undefined
  recip = undefined
  
  fromRational = Mu . const . fromRational
  
prod :: Num a => Mu m a -> Mu m a -> Mu m a
prod = liftM2 (*)

-- | 
asum :: Num a => Mu m a -> Mu m a -> Mu m a
asum p q = do 
  a <- p
  b <- q     
  return $ a + b - a * b

-- abelian
instance AdditiveGroup v where
  zeroV = empty
  (^+^) = asum
  negateV = undefined