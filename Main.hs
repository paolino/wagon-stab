{-# LANGUAGE 
    ViewPatterns
  , BangPatterns
  #-}

import Prelude hiding (minimum, maximum)
import Control.Exception (tryJust)
import Control.Monad (guard,forM_)
import System.IO.Error (isEOFError)

import Control.Lens (view)


import Data.List.Split (splitOn)

import Data.Machine.Moore (Moore (..))
import Core (operate, Machine (..), Parallels (..), Column (..), feed, machines)
import Lib (forceElems)
import Machines (minimum, maximum, counter, averageLength, average, lengths, occurs)


-- from header to Column, select machines depending on header type. Counter ispolymorph

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

-- read all next lines updating the column list on each turn. Return last update, force each Column and the list to whnf on each turn 

cycling :: [Column] -> IO [Column]
cycling !(forceElems -> cs) = 
  tryJust (guard . isEOFError) csv >>= 
     either (const $ return cs) (cycling . flip feed cs)

-- read first line to produce the booting [Column] value, cycle by reading line by line and print a report of the result
main = map column <$> csv >>= cycling >>= mapM_ output
