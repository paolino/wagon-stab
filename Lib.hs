
module Lib where
import Control.Parallel.Strategies
import Control.Parallel

-- | unforgiving zipWith, list lengths must match
zipWithU :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithU f [] [] = []
zipWithU f (x:xs) (y:ys) = f x y : zipWithU f xs ys
zipWithU _ _ _ = error "zipping on different length lists"

-- | force a list evaluation with elements 
forceElems :: [a] -> [a]
forceElems xs = f xs `pseq` xs where
  f [] = ()
  f (x:xs) = x `pseq` f xs
forceElemsPar = runEval . parListingWait where
  parListingWait :: [a] -> Eval [a]
  parListingWait [] = return []
  parListingWait (x:xs) = do
    x' <- rpar x
    xs' <- parListingWait xs
    x'' <- rseq x'
    return $ x'' : xs'




