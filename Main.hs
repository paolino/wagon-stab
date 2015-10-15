{-# LANGUAGE ViewPatterns, Rank2Types, ExistentialQuantification#-}

import Prelude hiding (minimum, maximum)
import Data.List (unfoldr)
import Text.Read
import Text.ParserCombinators.ReadP
import Control.Exception
import Control.Monad
import System.IO.Error
import Control.Arrow
import qualified Data.Map as M


import Data.List.Split



-------------------------------------------------
--- stateful operation type ----------------------
-----------------------------------------------

-- iso to a mealy machine (arrow composition is possible)
-- (https://github.com/paolino/sensors/blob/master/MealyT.hs)
-- Nothing is for null row fields, a is input, b is output
data Operation b a = Operation b (Maybe a -> Operation b a) 

--  a box for operations hiding result type, as we only need to report. We leave a as a parameter to make some spaghetti code later :(
data Machine a = forall b . (Show b) => Machine (Operation b a)

-- step the machine, feeding an input
operate ::  Maybe a -> Machine a -> Machine a
operate z (Machine (Operation x f)) = Machine .  f  $ z

-- couldn't find out the right strictness annotation
force (Machine (Operation r _)) = r `seq` return ()

------------------------------------------
--- machines ----------------------- 
------------------------------------------

-- count nulls and hits
-- if Machine hides the input type 'a' requesting a Read counter should nail down 'a' to some fake Readable datatype detecting null
data Counter = Counter {nulls :: Int, hit :: Int, totals :: Int} deriving Show

counter  :: Operation Counter a
counter = let 
  make n m = Operation (Counter n m (n + m)) $ f n m 
  f n m  Nothing = make (n + 1) m
  f n m  _ = make n (m + 1)
  in make 0 0

-----------------------------------------

-- track minimum value
data Minimum = Minimum (Maybe Float) deriving Show

minimum :: Operation Minimum Float
minimum = let 
  make m = Operation (Minimum m) $ f m 
  f Nothing x = make x
  f (Just m) (Just x) 
    | x < m = make $ Just x
    | otherwise = make $ Just m
  f m Nothing = make m
  in make Nothing
---------------------------------------------

-- track maximum
data Maximum = Maximum (Maybe Float) deriving Show

maximum :: Operation Maximum Float
maximum = let 
  make m = Operation (Maximum m) $ f m 
  f Nothing x = make x
  f (Just m) (Just x) 
    | x > m = make $ Just x
    | otherwise = make $ Just m
  f m Nothing = make m
  in make Nothing
-----------------------------------------------

-- track average (sum)
data Average = Average (Maybe Float) deriving Show

average :: Operation Average Float
average = let
  make n a = Operation (Average $ (/ fromIntegral n) `fmap` a ) $ f n a
  f n Nothing (Just x) = make (n + 1) (Just x)
  f n x Nothing = make n x
  f n (Just a) (Just x) = make (n + 1) (Just $ a + x)
  in make 0 Nothing
    
----------------------------------------------

-- track statistic. 
-- We could track the maximum, but as computing the minimum forces traversal, we compute maximum there also
data Occur = Occur String Int deriving Show
data Occurs = NoOccurs | Occurs {smallest::Occur, biggest ::Occur} deriving Show

-- fake String special Read as csv has unquoted strings
newtype Chars = Chars {unchars :: String}

instance Read Chars where
  readsPrec _ "" = []
  readsPrec _ x = [(Chars x,"")]


-- traverse statistic and compute the Occurs 
report :: M.Map String Int -> Occurs
report = M.foldrWithKey g NoOccurs  where
      g key count NoOccurs =  Occurs (Occur key count) (Occur key count)
      g key count o@(Occurs (Occur kmi mi) (Occur kma ma))
        | count > ma =   Occurs (Occur kmi mi) (Occur key count)
        | count == ma && kma < key =  Occurs (Occur kmi mi) (Occur key count)
        | count < mi =  Occurs (Occur key count) (Occur kma ma)
        | count == mi && kmi > key =  Occurs (Occur key count) (Occur kma ma)
        | otherwise = o

occurs :: Operation Occurs Chars
occurs = let
  make m  = Operation (report m) $ f m 
  f m  Nothing = make m 
  f m  (Just (Chars key))  =  make  $ M.alter (maybe (Just 1) (Just .(+1))) key m
  in make M.empty  
                
----------------------------------------------

-- track legths 
data Length = Length String Int deriving Show
data Lengths = NoLengths | Lengths {smallestL::Length, biggestL ::Length} deriving Show


lengths :: Operation Lengths Chars
lengths = let
  make o = Operation o $ f o
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
-- track average length (sum)
data AverageLength = AverageLength (Maybe Float) deriving Show

averageLength :: Operation AverageLength Chars
averageLength = let
  make n a = Operation (AverageLength $ (\x -> fromIntegral x / fromIntegral n ) `fmap` a ) $ f n a
  f n Nothing (Just (Chars x)) = make (n + 1) (Just $ length x)
  f n x Nothing = make n x
  f n (Just a) (Just (Chars x)) = make (n + 1) (Just $ a + length x)
  in make 0 Nothing

-------------------------------------------------------------------
--- end of machines ------------------------------------------
---------------------------------------------------------------

-- a type for different columns
-- the set of machines are intended to work in parallel, thus they share input type
data Column  
  = Numeric {title :: String , numberMachines :: [Machine Float]} 
  | Textual {title :: String,  stringMachines :: [Machine Chars]} 


-- spaghetti everywhere! (hide parameter of Machine with Read constraint ?)

forceColumn (Numeric _ ms) = mapM_ force ms -- this must be investigated, without it blows memory
forceColumn (Textual _ ms) = mapM_ force ms -- this must be investigated, without it blows memory

-- from comma separated strings to Column
parseColumns :: [String] -> [Column]
parseColumns = map (readField . break (== ' '))  where
  readField (tail -> name, init -> " (number)") = Numeric name [Machine counter, Machine minimum, Machine maximum, Machine average]
  readField (tail -> name, init -> " (text)")   = Textual name [Machine counter, Machine occurs, Machine lengths, Machine averageLength]


-- print the values of the machines 
output :: Column -> IO ()
output h = do
  putStrLn  ""
  putStrLn $ title  h
  putStrLn  ""
  case h of
          Numeric _ ms -> forM_ ms $ \(Machine (Operation r _)) -> do 
              putStr "-  ">> print r
          Textual _ ms -> forM_ ms $ \(Machine (Operation r _)) -> do 
              putStr "-  " >> print r

-- feed a line of values to the machines
feed :: [String] -> [Column] ->  [Column]
feed  = zipWith (\s -> q (operate . readMaybe $ s)) where
    q :: (forall a. Read a => Machine a -> Machine a) -> Column -> Column
    q f (Numeric n ms) = Numeric n (map f ms)
    q f (Textual n ms) = Textual n (map f ms)

-- get a line without return carriage
csv :: IO [String]
csv = splitOn "," <$> filter (/= '\r') <$> getLine


-- read all next lines updating the column list on each turn. Return last update
cycling :: [Column] -> IO [Column]
cycling cs = do 
  mapM_ forceColumn cs -- problem with lazyness
  tryJust (guard . isEOFError) csv >>= either (const $ return cs) (cycling . flip feed cs)

-- read first line to produce the booting [Column] value, cycle by reading line by line and print a report of the result
main = parseColumns <$> csv >>= cycling >>= mapM_ output
  
