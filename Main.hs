{-# LANGUAGE 
    ViewPatterns
  , ExistentialQuantification
  , TemplateHaskell
  , BangPatterns
  #-}

import Prelude hiding (minimum, maximum)
import Text.Read (readMaybe)
import Control.Exception (tryJust)
import Control.Monad (guard,forM_)
import System.IO.Error (isEOFError)
import Control.Arrow ((&&&))

import qualified Data.Map as M

import Control.Lens.TH (makeLenses)
import Control.Lens (over,view)


import Data.List.Split (splitOn)

import Data.Machine.Moore
import Core (operate, Machine (..))
import Machines

------------------------------------------
--- unforgiving zipWith ------------------
-----------------------------------------

zipWithU f [] [] = []
zipWithU f (x:xs) (y:ys) = f x y : zipWithU f xs ys
zipWithU _ _ _ = error "zipping on different length lists"



----------------------------------------------
---- forcing a list evaluation with elements -
----------------------------------------------

strictList xs = strictList' xs `seq` xs where
  strictList' [] = ()
  strictList' (x:xs) = x `seq` strictList' xs
------------------------------------------

--- a  contatiner for machines reading same input, should have a strict list as argument, so we force the spine and leave the element evaluation to stepParallels
data Parallels = forall a. Read a => Parallels ![Machine (Maybe a)]

-- step all machines of a Parallels with same input, forcing each Machine evaluation
step :: String -> Parallels -> Parallels
step s (Parallels ms) = Parallels $  strictList $ map (flip operate . readMaybe $ s) ms


-- the set of machines for a column are intended to work in parallel
data Column  = Column {title :: String , _machines :: !Parallels} 

makeLenses ''Column

-- feed a list of string to be parsed to the list of columns, step is parsing
feed :: [String] -> [Column] ->  [Column]
feed  =   zipWithU (over machines . step)  


-- from header to Column, assign the machines depending on header type
column :: String -> Column
column = (\(n,m) -> Column n $ parallels m) . break (== ' ') . init . tail  where
  parallels " (number)" =  Parallels $
    [Machine counter, Machine minimum, Machine maximum, Machine average ]
  parallels " (text)"   =  Parallels $ 
    [Machine counter, Machine lengths, Machine averageLength, Machine occurs]
  parallels _ = error "failed to parse a header"

-- print the values extracted from machines (should be json the class of r)
output :: Column -> IO ()
output h = do
  putStrLn  ""
  putStrLn $ title  h
  putStrLn  ""
  case view machines h of 
    Parallels ms -> forM_ ms $ 
      \(Machine (Moore r _)) -> putStr "-  ">> print r

  
-- get a line without return carriage
csv :: IO [String]
csv = splitOn "," <$> filter (/= '\r') <$> getLine

-- read all next lines updating the column list on each turn. Return last update, force each Column and the list to whnf 
cycling :: [Column] -> IO [Column]
cycling !(strictList -> cs) = tryJust (guard . isEOFError) csv >>= either (const $ return cs) (cycling . flip feed cs)

-- read first line to produce the booting [Column] value, cycle by reading line by line and print a report of the result
main = map column <$> csv >>= cycling >>= mapM_ output
