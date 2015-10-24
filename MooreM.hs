
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

{-# LANGUAGE 
    ViewPatterns
  , ExistentialQuantification
  , TemplateHaskell
  , BangPatterns
  #-}
import Data.List
import Data.Monoid

import Prelude hiding (minimum, maximum)
import Control.Arrow ((&&&), arr)

import qualified Data.Map as M
import Control.Parallel.Strategies
import Control.Monad
import Lib

data Minimum = Minimum !(Maybe Integer) deriving Show

instance Monoid Minimum where
  Minimum (Just !x) `mappend` Minimum (Just !y) = Minimum . Just $  min x y
  Minimum Nothing `mappend` Minimum Nothing = Minimum Nothing
  Minimum x `mappend` Minimum Nothing = Minimum x
  _ `mappend` y = y
  mempty = Minimum Nothing


digest :: Monoid m => Int -> (m -> a -> m) -> m -> [a] -> m 
digest n i x = foldl' mappend mempty .  withStrategy (parListWait) . zipWith (foldl' i) (x : repeat mempty) . chunksOf n 
chunksOf m xs = chunksOf' m xs  where
  chunksOf' n [] = [[]]
  chunksOf' 0 xs = []:chunksOf' m xs 
  chunksOf' n (x:xs) = let 
    (h:r) = chunksOf' (n - 1) xs
    in (x:h):r
main = do
  let datas = map Just $ [100000000,100000000-1..1]
  print $ digest (1000000) (\m -> mappend m . Minimum) mempty datas
  
{-


data MoorePar a b = forall m . Monoid m => MoorePar  (m -> b) (m -> a -> m) !m

digest :: Int -> MoorePar a b -> [a] -> MoorePar a b 
digest n (MoorePar o i x ) = MoorePar o i . foldl1 mappend . withStrategy (parList rseq) . zipWith (foldl' i) (x : repeat mempty) . chunksOf n 

result :: MoorePar a b -> b
result (MoorePar o i x) = o x

-- count nulls and hits
data Counter = Counter {nulls :: Int, hits :: Int, total :: Int} deriving Show

data Count = Count !Int !Int 

instance Monoid Count where
  mempty = Count 0 0
  mappend (Count n1 m1) (Count n2 m2) = Count (n1 + n2) (m1 + m2)

fromCount (Count n m) = Counter n m (n + m)

counter  :: MoorePar (Maybe a) Counter
counter = MoorePar fromCount f mempty  where
  f (Count n m) Nothing = Count (n + 1) m
  f (Count n m) _ = Count n (m + 1)


feed :: Int -> [Moore a ] -> [[a]] -> [Moore a ]
feed n m = map (\(Moore m) -> Moore . foldl (digest n)) m

data Moore a = forall b . Show b => Moore (MoorePar a b)

main = do
  let datas = chunksOf 600 $ map Just [1..50000000]
  print . map (show . result) $ feed 100 [Moore counter, Moore minimum'] $ datas
-----------------------------------------

-- track minimum value

data Minimum = Minimum !(Maybe Float) deriving Show

instance Monoid Minimum where
  mappend = liftM2 min
  mempty = Nothing


minimum' :: MoorePar  (Maybe Float) Minimum 
minimum' = MoorePar mappend id Nothing 
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
