{-# LANGUAGE ViewPatterns #-}
module Data.Fuzzy.Plot where

import Data.List
import Data.Fuzzy.Mu

plot :: (Ord a, Enum a, Num a, Show a) => a -> [a] -> IO ()
plot scale xxs@(map (scale *) -> xs) = do 
  print $ maximum xxs
  mapM_ putStrLn $ transpose $ map list xs
  print $ minimum xxs
  where list x = let n = fromEnum x in
          replicate (maxV - n) ' ' ++ "*" ++ replicate (n - minV)  ' ' 
        maxV = fromEnum $ maximum xs
        minV = fromEnum $ minimum xs

scan :: (Num a, Enum a) => (a -> a) -> (a, a, a) -> [a]
scan f (a, s, b) = map f $ enumFromThenTo a (a + s) b

plotf :: (Enum a, Ord a, Num a, Show a) => (a -> a) -> a -> (a, a, a) -> IO ()
plotf mu scale = plot scale . scan mu

plotMu (Mu mu) = plotf mu 50

(.%.) :: (Enum a, Ord a, Num a, Show a) => (a, a, a) -> Mu a a -> IO ()
(.%.) = flip plotMu 

infix 0 .%.