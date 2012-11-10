
module Test.Laws 
       ( prop_leftIdentity, prop_rightIdentity, prop_identity, prop_annihilation
       , prop_associative, prop_commutative, prop_monotonic
       , prop_monoid, prop_Monoid 
       , prop_tnorm, prop_Tnorm
       , prop_tconorm, prop_Tconorm
       , T(..)
       ) where

import Data.Tnorm
import Data.Tconorm
import Test.PropBox 

import Test.QuickCheck 
import Data.Monoid (Monoid (..))

prop_leftIdentity :: (Eq a, Show a, Arbitrary a) => a -> (a -> a -> a) -> PropBox
prop_leftIdentity iden f = atomProp "left identity" prop
  where prop x = f iden x == x
        
prop_rightIdentity :: (Eq a, Show a, Arbitrary a) => a -> (a -> a -> a) -> PropBox
prop_rightIdentity iden f = atomProp "rigth identity" prop
  where prop x = f x iden == x

prop_identity :: (Eq a, Show a, Arbitrary a) => a -> (a -> a -> a) -> PropBox
prop_identity iden f = propGroup "identity" [prop_leftIdentity iden f, prop_rightIdentity iden f]

prop_associative :: (Eq a, Show a, Arbitrary a) => (a -> a -> a) -> PropBox
prop_associative f = atomProp "associative" prop
  where prop a b c = f a (f b c) == f (f a b) c

prop_commutative :: (Eq b, Show a, Arbitrary a) => (a -> a -> b) -> PropBox
prop_commutative f = atomProp "commutative" prop
  where prop a b = f a b == f b a

prop_monotonic :: (Ord a, Show a, Arbitrary a) => (a -> a -> a) -> PropBox
prop_monotonic f = atomProp "monotonic" prop
  where prop a b c d = f (min a c) (min b d) <= f (max a c) (max b d)

prop_annihilation :: (Eq a, Show a, Arbitrary a) => a -> (a -> a -> a) -> PropBox
prop_annihilation an f = atomProp "annihilation" prop
  where prop x = f an x == an && an == f x an

data T a = T

prop_monoid :: (Eq a, Show a, Arbitrary a) => a -> (a -> a -> a) -> PropBox
prop_monoid iden f = propGroup "monoid" [ prop_associative f , prop_identity iden f ]

prop_Monoid :: (Eq a, Show a, Arbitrary a, Monoid a) => T a -> PropBox
prop_Monoid t = prop_monoid (tie t) mappend
  where tie :: Monoid a => T a -> a
        tie _ = mempty

prop_tnorm :: (Ord a, Num a, Show a, Arbitrary a) => (a -> a -> a) -> PropBox
prop_tnorm f = propGroup "t-norm" 
                      [ prop_monoid       1 f 
                      , prop_commutative    f      
                      , prop_monotonic      f  
                      , prop_annihilation 0 f 
                      ]

prop_Tnorm :: (Show a, Arbitrary a, Tnorm a) => T a -> PropBox
prop_Tnorm = prop_tnorm . tie
  where tie :: Tnorm a => T a -> (a -> a -> a)
        tie _ = tnorm
        
prop_tconorm :: (Show a, Arbitrary a, Tconorm a) => (a -> a -> a) -> PropBox
prop_tconorm f = propGroup "t-conorm"
                      [ prop_monoid       0 f 
                      , prop_commutative    f      
                      , prop_monotonic      f  
                      , prop_annihilation 1 f 
                      ]

prop_Tconorm :: (Show a, Arbitrary a, Tconorm a) => T a -> PropBox
prop_Tconorm = prop_tconorm . tie
  where tie :: Tconorm a => T a -> (a -> a -> a)
        tie _ = snorm
