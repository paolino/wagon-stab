
module Lib where

import Data.Machine.Moore
import Data.Machine.Mealy

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

-- | insert a Mealy machine in a Moore
glue :: Mealy a b -> Moore b c -> Moore a c
glue (Mealy me) (Moore r mo) = Moore r (\x -> let 
    (y,me') = me x
    in glue me' $ mo y 
    )

-- | infix glue
(|->) ::  Mealy a b -> Moore b c -> Moore a c
(|->) = glue

