{-# LANGUAGE 
    ViewPatterns
  , BangPatterns
  , ScopedTypeVariables
  #-}

import Prelude hiding (minimum, maximum)
import Control.Exception (tryJust)
import Control.Monad (guard,forM_)
import System.IO.Error (isEOFError)
import System.Environment (getArgs)
import Control.Lens (view,over,_1)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe)


import Data.List.Split (splitOn)

import Data.Machine.Moore (Moore (..))
import Core (operate, Machine (..), Parallels (..), Column (..), feed, machines, multifeed)
import Lib (forceElemsPar, forceElems')
import Machines (minimum, maximum, counter, averageLength, average, lengths, occurs)
import Control.Monad
import Control.Parallel.Strategies
import Control.Parallel

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

  

cycling :: Int -> Int -> [Column] -> IO [Column]
cycling nc n !cs = let 
  cs' = (if n `mod` nc == 0 then  forceElemsPar   else id) $ cs
  in tryJust (guard . isEOFError) csv >>=   
     either (const $ return cs) ( cycling nc (n + 1). flip feed cs')

multicycling :: Int -> [Column] -> IO [Column]
multicycling nc !cs = do
  let untilEOF n 
                | n > nc = return ([],False)
                | otherwise = do
                    line <- tryJust (guard . isEOFError) csv
                    case line of 
                        Left _ -> return ([],True)
                        Right x -> over _1 (x:) <$> untilEOF (n + 1)
  (ls :: [[String]],out) <- untilEOF 0
  let cs' = forceElemsPar (multifeed ls cs)
  if null ls then return cs 
    else if out then return cs'
      else multicycling nc $ forceElems' $ cs'
  


-- read first line to produce the booting [Column] value, cycle by reading line by line and print a report of the result
main = do
  nc <- (fromMaybe 1 . (listToMaybe  >=> readMaybe)) <$>  getArgs
  map column <$> csv >>= multicycling nc >>= mapM_ output
