
module Lib where

-- | unforgiving zipWith, list lengths must match
zipWithU :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithU f [] [] = []
zipWithU f (x:xs) (y:ys) = f x y : zipWithU f xs ys
zipWithU _ _ _ = error "zipping on different length lists"

-- | force a list evaluation with elements 
forceElems :: [a] -> [a]
forceElems xs = f xs `seq` xs where
  f [] = ()
  f (x:xs) = x `seq` f xs



