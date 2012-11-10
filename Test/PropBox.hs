{-# LANGUAGE DeriveFunctor #-}
module Test.PropBox (PropName, PropBox, atomProp, propGroup, boxTests) where
       
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Testable(..), Property)

-- | Labeled Leaf Tree.
data LLTree label leaf = Branches label [LLTree label leaf]
                       | Leaf     label leaf
                         deriving (Eq, Ord, Show, Read, Functor)

flatten :: (label -> [a] -> a) -> (label -> leaf -> a) -> LLTree label leaf -> a
flatten g f (Branches label brns) = g label $ map (flatten g f) brns
flatten _ f (Leaf     label leaf) = f label leaf

-- | Property label.
type PropName   = String

-- | Repersent complex hierarchical properties.
type PropBox    = LLTree PropName Property 

-- | Makes an atomic labeled property.
atomProp :: Testable prop => PropName -> prop -> PropBox
atomProp name = Leaf name . property

-- | Makes a labeled property group.
propGroup :: PropName -> [PropBox] -> PropBox
propGroup = Branches

-- | Makes a hierarchical test with corrensponding labels.
boxTests :: PropBox -> Test
boxTests = flatten testGroup testProperty 

