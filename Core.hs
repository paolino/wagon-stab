{-# LANGUAGE 
   ExistentialQuantification
  , BangPatterns
  #-}
module Core where

import Data.Machine.Moore
import Data.Machine.Mealy
import Control.Arrow

-- final filter boxes are Moore machines 
-- (which is not a Category instance :-()
-- (it's a moore because it exposes its state b, never the less they can share a common circuitry of mealy producing their input where we don't need to expose state)
-- b is output type, implementation are responsible to make evaluations run enough



--  a box for operations hiding result type, as we only need to report. (Show, should be JSON)

data Machine a = forall b . Show b => Machine (Moore  a b)

-- step the machine, feeding an input, force the output to be evaluated
operate ::  Machine a -> a -> Machine a
operate (Machine (Moore !x f)) = Machine .  f  

glue :: Mealy a b -> Moore b c -> Moore a c
glue (Mealy me) (Moore r mo) = Moore r (\x -> let 
    (y,me') = me x
    in glue me' $ mo y 
    )



