{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}
import Data.Tnorm
import Data.Tconorm

import Test.PropBox
import Test.Laws

import Test.Framework (defaultMain, testGroup, Test)
import Test.QuickCheck        as QC 

import Control.Applicative ((<$>))


newtype DoubleN = DoubleN Double
                  deriving (Show, Read, Eq, Num, Ord, Fractional)

instance Arbitrary DoubleN where
  arbitrary = DoubleN <$> choose (0, 1)

-- instances of Arbitrary for Tnorm instances
deriving instance Arbitrary a => Arbitrary (Min a)  
deriving instance Arbitrary a => Arbitrary (Product a)
deriving instance Arbitrary a => Arbitrary (Lukasiewicz a)
deriving instance Arbitrary a => Arbitrary (Drastic a)
deriving instance Arbitrary a => Arbitrary (NilpotentMin a)
deriving instance Arbitrary a => Arbitrary (HamacherProd a)

-- instances of Arbitrary for Tconorm instances
deriving instance Arbitrary a => Arbitrary (Max a)
deriving instance Arbitrary a => Arbitrary (ProbSum a)
deriving instance Arbitrary a => Arbitrary (BoundedSum a)
deriving instance Arbitrary a => Arbitrary (Codrastic a)
deriving instance Arbitrary a => Arbitrary (NilpotentMax a)
deriving instance Arbitrary a => Arbitrary (EinsteinSum a)

test_both :: (Show (t a), Arbitrary (t a), Tnorm (t a), 
             Show ((DualTconorm t) a), 
             Arbitrary ((DualTconorm t) a), 
             Tconorm ((DualTconorm t) a))
             => T (t a) -> [PropBox]
test_both t = [prop_Tnorm t, prop_Tconorm (tie t)]
  where tie :: T (t a) -> T ((DualTconorm t) a)
        tie = error "tie: test_both"
  
tnormTests :: PropBox
tnormTests = propGroup "t-norms" $ concat
                 [ test_both (T :: T (Min DoubleN)) 
                 , test_both (T :: T (Product DoubleN)) 
                 , test_both (T :: T (Lukasiewicz DoubleN)) 
                 , test_both (T :: T (Drastic DoubleN)) 
                 , test_both (T :: T (NilpotentMin DoubleN)) 
                 , test_both (T :: T (HamacherProd DoubleN)) 
                 ]

main :: IO ()
main = defaultMain $ map boxTests [ tnormTests]
