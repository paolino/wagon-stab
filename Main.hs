{-# LANGUAGE ViewPatterns, Rank2Types, ExistentialQuantification, TemplateHaskell, BangPatterns#-}

import Prelude hiding (minimum, maximum)
import Data.List (unfoldr)
import Text.Read
import Text.ParserCombinators.ReadP
import Control.Exception
import Control.Monad
import System.IO.Error
import Control.Arrow
import qualified Data.Map as M
import Control.Lens.TH
import Control.Lens


import Data.List.Split



----------------------------------------------
---- forcing a list evaluation with elements
----------------------------------------------

strictList xs = strictList' xs `seq` xs where
  strictList' [] = ()
  strictList' (x:xs) = x `seq` strictList' xs


-------------------------------------------------
--- stateful operation type ----------------------
-----------------------------------------------

-- iso to a mealy machine (Category instance)
-- (https://github.com/paolino/sensors/blob/master/MealyT.hs)
-- Nothing is for null row fields, a is input, b is strict output

data Operation b a = Operation !b (Maybe a -> Operation b a) 

--  a box for operations hiding result type, as we only need to report. Strict in the Operation to force output

data Machine a = forall b . (Show b) => Machine !(Operation b a)

-- step the machine, feeding an input

operate ::  Maybe a -> Machine a -> Machine a
operate z (Machine (Operation x f)) = Machine .  f  $ z

------------------------------------------
--- machines ----------------------- 
------------------------------------------

-- count nulls and hits
-- if Machine hides the input type 'a' requesting a Read counter should nail down 'a' to some fake Readable datatype detecting null

data Counter = Counter {nulls :: Int, hits :: Int, total :: Int} deriving Show

counter  :: Operation Counter a
counter = let 
  make !n !m = Operation (Counter n m (n + m)) $ f n m 
  f n m  Nothing = make (n + 1) m
  f n m  _ = make n (m + 1)
  in make 0 0

-----------------------------------------

-- track minimum value
data Minimum = Minimum !(Maybe Float) deriving Show

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
data Maximum = Maximum !(Maybe Float) deriving Show

maximum :: Operation Maximum Float
maximum = let 
  make m = Operation (Maximum m) $ f m 
  f Nothing x = make x
  f (Just m) (Just x) 
    | x > m = make $ Just x
    | otherwise =  make $ Just m
  f m Nothing = make m
  in make Nothing
-----------------------------------------------

-- track average (sum)
data Average = Average (Maybe Float) deriving Show

average :: Operation Average Float
average = let
  make !n a = Operation (Average $ (/ fromIntegral n) `fmap` a ) $ f n a
  f n Nothing (Just x) = make (n + 1) (Just x)
  f n x Nothing = make n x
  f n (Just !a) (Just x) = make (n + 1) (Just $ a + x)
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


-- traverse statistic and compute the Occurs, to force Map evaluation we read the min key on each insert  
report :: M.Map String Int -> Occurs
report m = r `seq` M.foldrWithKey g NoOccurs m where
      g key count NoOccurs =  Occurs (Occur key count) (Occur key count) 
      g key count o@(Occurs (Occur kmi mi) (Occur kma ma) )
        | count > ma =   Occurs (Occur kmi mi) (Occur key count) 
        | count == ma && kma < key =  Occurs (Occur kmi mi) (Occur key count) 
        | count < mi =  Occurs (Occur key count) (Occur kma ma) 
        | count == mi && kmi > key =  Occurs (Occur key count) (Occur kma ma) 
        | otherwise = o
      (_,r) = if M.size m > 0 then M.findMin m else (undefined,0)

occurs :: Operation Occurs Chars
occurs = let
  make m  = Operation (report m) $ f m 
  f m  Nothing = make m 
  f m  (Just (Chars key))  =  make  $ M.alter (maybe (Just 1) (Just .(+1))) key m
  in make M.empty  
                
----------------------------------------------

-- track legths 
data Length = Length !String !Int deriving Show
data Lengths = NoLengths | Lengths {smallestL :: !Length, biggestL :: !Length} deriving Show


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
data AverageLength = AverageLength !(Maybe Float) deriving Show

averageLength :: Operation AverageLength Chars
averageLength = let
  make !n a = Operation (AverageLength $ (\x -> fromIntegral x / fromIntegral n ) `fmap` a ) $ f n a
  f n Nothing (Just (Chars x)) = make (n + 1) (Just $ length x)
  f n x Nothing = make n x
  f n (Just !a) (Just (Chars x)) = make (n + 1) (Just $ a + length x)
  in make 0 Nothing

-------------------------------------------------------------------
--- end of machines ------------------------------------------
---------------------------------------------------------------

--- a  contatiner for machines reading same input, should have a strict list as argument, so we force the spine and leave the element evaluation to stepParallels
data Parallels = forall a. Read a => Parallels ![Machine a]

-- step all machines of a Parallels with same input, forcing Machines evaluation
stepParallels :: String -> Parallels -> Parallels
stepParallels s (Parallels ms) = Parallels $  strictList $ map (operate . readMaybe $ s) ms


-- a type for different columns
-- the set of machines are intended to work in parallel, thus they share input type, strict in the _machine field
data Column  
  = Numeric {title :: String , _machines :: !Parallels} 
  | Textual {title :: String,  _machines :: !Parallels} 

makeLenses ''Column




-- from comma separated strings to Column
parseColumns :: [String] -> [Column]
parseColumns = map (readField . break (== ' '))  where
  -- readField (tail -> name, init -> " (number)") = Numeric name . Parallels $ [Machine counter :: Machine Float , Machine minimum, Machine maximum, Machine average]
  readField (tail -> name, init -> " (number)") = Numeric name . Parallels $ [Machine counter :: Machine Float, Machine minimum, Machine maximum, Machine average ]
  readField (tail -> name, init -> " (text)")   = Textual name . Parallels $ [Machine counter :: Machine Chars , Machine lengths, Machine averageLength, Machine occurs]
  -- readField (tail -> name, init -> " (text)")   = Textual name . Parallels $ [Machine counter :: Machine Chars , Machine occurs, Machine lengths, Machine averageLength]


-- print the values of the machines 
output :: Column -> IO ()
output h = do
  putStrLn  ""
  putStrLn $ title  h
  putStrLn  ""
  case view machines h of
    Parallels ms -> forM_ ms $ 
      \(Machine (Operation r _)) -> putStr "-  ">> print r

-- feed a line of values to the machines
feed :: [String] -> [Column] ->  [Column]
feed  ss cs =   zipWith (over machines . stepParallels)  ss cs 
  

-- get a line without return carriage
csv :: IO [String]
csv = splitOn "," <$> filter (/= '\r') <$> getLine


-- read all next lines updating the column list on each turn. Return last update, force each Column and the list to whnf 
cycling :: [Column] -> IO [Column]
cycling !(strictList -> cs) = tryJust (guard . isEOFError) csv >>= either (const $ return cs) (cycling . flip feed cs)

-- read first line to produce the booting [Column] value, cycle by reading line by line and print a report of the result
main = parseColumns <$> csv >>= cycling >>= mapM_ output
