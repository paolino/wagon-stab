{-# LANGUAGE 
   ExistentialQuantification
  , BangPatterns
  , TemplateHaskell
  , ViewPatterns
  #-}
module Core where

import Data.Machine.Moore
import Data.Machine.Mealy
import Control.Arrow
import Control.Lens
import Control.Lens.TH
import Lib (forceElems, zipWithU, forceElemsPar)
import Text.Read (readMaybe)
import Data.List

-- final filter boxes are Moore machines 
-- (which is not a Category instance :-()
-- (it's a moore because it exposes its state b, never the less they can share a common circuitry of mealy producing their input where we don't need to expose state)
-- b is output type, implementation are responsible to make evaluations run enough



--  a box for operations hiding result type, as we only need to report. (Show, should be JSON)
data Machine a = forall b . Show b => Machine (Moore a b)

-- step the machine, feeding an input, force the output to be evaluated
operate ::  Machine a -> a -> Machine a
operate (Machine (Moore !x f)) = Machine .  f  

multioperate :: Machine a -> [a] -> Machine a
multioperate (Machine m@(Moore !x _)) = Machine . foldl k m where
  k (Moore x f) = f 
--- a  contatiner for machines reading same input, should have a strict list as argument, so we force the spine and leave the element evaluation to stepParallels
data Parallels = forall a. Read a => Parallels [Machine (Maybe a)]

-- step all machines of a Parallels with same input , parsed, forcing each Machine evaluation
step :: String -> Parallels -> Parallels
step s (Parallels !(forceElems -> ms)) = Parallels $ map ( flip operate . readMaybe $ s) ms

multistep :: [String] -> Parallels -> Parallels
multistep xs (Parallels !(forceElemsPar -> ms)) = Parallels $ map (flip multioperate xs') ms
  where xs' = forceElems $ map readMaybe $ xs

-- the set of machines for each column are intended to work in parallel
data Column  = Column {title :: String , _machines :: !Parallels} 

makeLenses ''Column

-- feed a list of string to be parsed to the list of columns, step is doing parsing
feed :: [String] -> [Column] ->  [Column]
feed  = zipWithU (over machines . step)  

multifeed :: [[String]] -> [Column] -> [Column]
multifeed xs = zipWithU (over machines . multistep) (transpose xs)
