
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

{-# LANGUAGE 
    ViewPatterns
  , ExistentialQuantification
  , TemplateHaskell
  , BangPatterns
  #-}
import Data.List
import Data.List.Split
import Data.Monoid

import Prelude hiding (minimum, maximum)
import Control.Arrow ((&&&), arr)

import qualified Data.Map as M
import Control.Parallel.Strategies


data Moore m a b = Moore {
    state :: !m
  , output :: m -> b
  , accept :: m -> a -> Moore m a b
  }



digestM :: [a] -> Moore m a  b -> Moore m a  b
digestM  = flip (foldl (\m x -> accept m (state m) x))


digestMs :: Monoid m => [[a]] -> Moore m a b -> Moore m a  b
digestMs xs m = foldl1 f . withStrategy (parList rseq) . zipWith digestM xs $ m : repeat m{state = mempty} where
  f !m !m' =  m{state = state m `mappend` state m'}


data MoorePar a b = forall m . Monoid m => MoorePar !(Moore m a b)

digest :: Int -> [a] -> MoorePar a b -> MoorePar a b
digest n xs (MoorePar !m) = MoorePar . flip digestMs m . chunksOf n $ xs

result :: MoorePar a b -> b
result (MoorePar m) = output m (state m)

-- count nulls and hits
data Counter = Counter {nulls :: Int, hits :: Int, total :: Int} deriving Show

data Count = Count !Int !Int 

instance Monoid Count where
  mempty = Count 0 0
  (Count n1 m1) `mappend` (Count n2 m2) = Count (n1 + n2) (m1 + m2)

fromCount (Count n m) = Counter n m (n + m)

counter  :: Moore Count (Maybe a) Counter
counter = let 
  make !c = Moore c fromCount f 
  f (Count n m) Nothing = make $ Count (n + 1) m
  f (Count n m) _ = make $ Count n (m + 1)
  in make mempty


feed :: Int -> MoorePar a b -> [[a]] -> MoorePar a b
feed n m = foldl (flip $ digest n)  m

main = print . result $ feed 100 (MoorePar counter)  (chunksOf 400 $ map Just [1..50000000]) 
{-
-----------------------------------------

-- track minimum value

data Minimum = Minimum !(Maybe Float) deriving Show

minimum :: Moore (Maybe Float) Minimum 
minimum = let 
  make m = Moore (Minimum m) $ f m 
  f Nothing x = make x
  f (Just m) (Just x) 
    | x < m = make $ Just x
    | otherwise = make $ Just m
  f m Nothing = make m
  in make Nothing
---------------------------------------------

-- track maximum value

data Maximum = Maximum !(Maybe Float) deriving Show

maximum :: Moore (Maybe Float) Maximum 
maximum = let 
  make m = Moore (Maximum m) $ f m 
  f Nothing x = make x
  f (Just m) (Just x) 
    | x > m = make $ Just x
    | otherwise =  make $ Just m
  f m Nothing = make m
  in make Nothing
-----------------------------------------------

-- track sum and count (should have a second input from Counter, composition, avoid counting twice), expose average

data Average = Average (Maybe Float) deriving Show

average :: Moore (Maybe Float) Average
average = let
  make !n a = Moore (Average $ (/ fromIntegral n) `fmap` a ) $ f n a
  f n Nothing (Just x) = make (n + 1) (Just x)
  f n x Nothing = make n x
  f n (Just !a) (Just x) = make (n + 1) (Just $ a + x)
  in make 0 Nothing
    
----------------------------------------------

-- track statistic, exposing extremants

-- We could track the maximum, but as computing the minimum forces traversal, we compute maximum there also
data Occur = Occur String Int deriving Show
data Occurs = NoOccurs | Occurs {smallest::Occur, biggest ::Occur} deriving Show

-- fake String special Read as csv has unquoted strings
newtype Chars = Chars {unchars :: String}

instance Read Chars where
  readsPrec _ "" = []
  readsPrec _ x = [(Chars x,"")]


-- traverse statistic and compute the Occurs lazily, to force Map evaluation we read and seq the min value on each insert  
report :: M.Map String Int -> Occurs
report m = r `seq`  M.foldrWithKey g NoOccurs m where
      g key count NoOccurs =  Occurs (Occur key count) (Occur key count) 
      g key count o@(Occurs (Occur kmi mi) (Occur kma ma) )
        | count > ma =   Occurs (Occur kmi mi) (Occur key count) 
        | count == ma && kma < key =  Occurs (Occur kmi mi) (Occur key count) 
        | count < mi =  Occurs (Occur key count) (Occur kma ma) 
        | count == mi && kmi > key =  Occurs (Occur key count) (Occur kma ma) 
        | otherwise = o
      (_,r) = if M.size m > 0 then M.findMin m else (undefined,0)

occurs :: Moore (Maybe Chars) Occurs 
occurs = let
  make m  = Moore (report m) $ f m 
  f m  Nothing = make m 
  f m  (Just (Chars key))  =  make $ M.alter (maybe (Just 1) (Just .(+1))) key m
  in make M.empty  
                
----------------------------------------------

-- track extremant lengths 

data Length = Length !String !Int deriving Show
data Lengths = NoLengths | Lengths {smallestL :: !Length, biggestL :: !Length} deriving Show


lengths :: Moore (Maybe Chars) Lengths 
lengths = let
  make o = Moore o $ f o
  f o Nothing = make  o
  f NoLengths  (Just (Chars key)) = make $ Lengths (Length key count) (Length key count) where count  = length key
  f o@(Lengths (Length kmi mi) (Length kma ma)) (fmap (id &&& length . unchars) -> Just (Chars key, count)) 
        | count > ma = make $  Lengths (Length kmi mi) (Length key count)
        | count == ma && kma < key = make $ Lengths (Length kmi mi) (Length key count)
        | count < mi = make $  Lengths (Length key count) (Length kma ma)
        | count == mi && kmi > key = make $ Lengths (Length key count) (Length kma ma)
        | otherwise = make o

  in make NoLengths 
                
--------------------------------------

-- track sum length , expose average length

data AverageLength = AverageLength !(Maybe Float) deriving Show

averageLength :: Moore (Maybe Chars) AverageLength 
averageLength = let
  make !n a = Moore (AverageLength $ (\x -> fromIntegral x / fromIntegral n ) `fmap` a ) $ f n a
  f n Nothing (Just (Chars x)) = make (n + 1) (Just $ length x)
  f n x Nothing = make n x
  f n (Just !a) (Just (Chars x)) = make (n + 1) (Just $ a + length x)
  in make 0 Nothing





-}
