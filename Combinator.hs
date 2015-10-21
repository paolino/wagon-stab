{-# LANGUAGE 
    ViewPatterns
  , ExistentialQuantification
  , TemplateHaskell
  , BangPatterns
  #-}
  
-- experimenting insertion of mealy in moores
-- module Combinator where

import Core
import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow 
import Control.Lens
import Control.Lens.TH
import qualified Data.Map as M
import Data.Ord
import Data.List

import Data.Machine.Mealy
import Data.Machine.Moore


maxi :: Mealy (M.Map a Int) a
maxi = arr (fst . maximumBy (comparing snd) . M.assocs)

occurs :: Ord a => Mealy a (M.Map a Int)
occurs = let 
  make m = Mealy $ \key -> let
          m' = M.alter (maybe (Just 1) (Just .(+1))) key m
          in (m', make m')
  in make M.empty

count :: Mealy a Int
count = let
  make n = Mealy $ \_ -> (n + 1, make $ n + 1)
  in make 0


-- | insert a Mealy machine in a Moore
glue :: Mealy a b -> Moore b c -> Moore a c
glue (Mealy me) (Moore r mo) = Moore r (\x -> let 
    (y,me') = me x
    in glue me' $ mo y 
    )
-- | infix glue
(|->) ::  Mealy a b -> Moore b c -> Moore a c
(|->) = glue
-- switch based on Maybe values
switchJust :: Mealy a b -> Mealy () c -> Mealy (Maybe a) (Either b c)
switchJust (Mealy pos) (Mealy neg) = Mealy f where
  f Nothing = let (b,neg') = neg ()
    in (Right b,switchJust (Mealy pos) neg')
  f (Just x) = let (b,pos') = pos x
    in (Left b,switchJust pos' (Mealy neg))

data Report a = Report
  {
  _nulls :: Int,
  _hits :: Int,
  _maxout :: Maybe a
  }

makeLenses ''Report

keep :: Moore (Either (a,Int) Int) (Report a)
keep = let
  make  r = Moore r (make . ($ r) . either bset (set nulls))
  bset (x,i) = set hits i . set maxout (Just x)
  in make (Report 0 0 Nothing)

report :: Moore (Maybe Float) (Report Float)
report = switchJust (maxi . occurs &&& count) count |-> keep

