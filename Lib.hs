
module Lib where
import Control.Parallel.Strategies
import Control.Parallel

-- | unforgiv zipWith, list lengths must match
zipWithU :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithU f [] [] = []
zipWithU f (x:xs) (y:ys) = f x y : zipWithU f xs ys
zipWithU _ _ _ = error "zipp on different length lists"

-- | force a list evaluation with elements 
forceElems :: [a] -> [a]
forceElems xs = f xs `pseq` xs where
  f [] = ()
  f (x:xs) = x `pseq` f xs
forceElemsPar = runEval . parListWait where
  parListWait :: [a] -> Eval [a]
  parListWait [] = return []
  parListWait (x:xs) = do
    x' <- rpar x
    xs' <- parListWait xs
    x'' <- rseq x'
    xs'' <- rseq xs'
    return $ x'' : xs''

forceElems' = runEval . parListWait where
  parListWait :: [a] -> Eval [a]
  parListWait [] = return []
  parListWait (x:xs) = do
    x' <- rseq x
    xs' <- parListWait xs
    return $ x' : xs'



