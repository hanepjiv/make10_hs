{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10 ( Operator(..)
              , function
                -- -------------------------------------------------------------
              , Cell(..)
              , apply
                -- -------------------------------------------------------------
              , select
              , combinations
              ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Arrow

import Make10.Operator
import Make10.Cell
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings

-- =============================================================================
-- -----------------------------------------------------------------------------
isNull :: [a] -> Either [a] [a]
isNull [] = Left  []
isNull x  = Right x
-- -----------------------------------------------------------------------------
-- | select
--
-- >>> select []
-- []
--
-- >>> select [0,1]
-- [(0,[1]),(1,[0])]
--
-- >>> select [0,1,2]
-- [(0,[1,2]),(1,[0,2]),(2,[0,1])]
select :: forall c. [c] -> [(c, [c])]
{-
select []       = []
-- select (x:xs)   = (x,xs) : map (\ (y,ys) -> (y,x:ys)) (select xs)
select (x:xs)   = (x,xs) : map (second ((:) x))       (select xs)
-}
select =
  loop $ (snd &&& fst >>> app) &&&
  ((<<< isNull)
   <<< (const [] |||)
   <<< (uncurry (:) <<<)
   <<< (<<< (head &&& tail))
   <<< (id &&&)
   <<< (uncurry map <<<)
   <<< ((second <<< (:) <<< fst) &&&)
   <<< (<<< snd)
   <<< snd)
-- -----------------------------------------------------------------------------
-- | combinations
--
-- >>> combinations 1 []
-- []
--
-- >>> combinations 1 [0,1]
-- [[0],[1]]
--
-- >>> combinations 2 [0,1,2]
-- [[0,1],[0,2],[1,2]]
combinations :: forall t. Integer -> [t] -> [[t]]
{-
combinations _ []     = []
combinations 0 _      = []
combinations 1 x      = map (: []) x
combinations n (x:xs) = map (x:) (combinations (pred n) xs) ++ combinations n xs
-}
combinations =
  curry $ loop $ (snd &&& fst >>> app) &&&
  ((<<< isSndNull)
   <<< (const [] |||)
   <<< (<<< isFstZero)
   <<< (const [] |||)
   <<< (<<< isFstOne)
   <<< ((map (: []) <<< snd) |||)
   <<< (uncurry (++) <<<)
   <<< uncurry (&&&)
   <<< (((uncurry map <<<)
         <<< (((:) <<< head <<< snd) &&&)
         <<< (<<< ((pred <<< fst) &&& (tail <<< snd))))
        &&& (<<< (fst &&& (tail <<< snd))))
   <<< snd)
  where
    -- -------------------------------------------------------------------------
    isSndNull        :: forall t1 t2.
                        (t1, [t2])      -> Either (t1, [t2]) (t1, [t2])
    isSndNull           a@(_, [])       =  Left  a
    isSndNull           a               =  Right a
    -- -------------------------------------------------------------------------
    isFstZero        :: forall t1 t2. (Num t1, Eq t1) =>
                        (t1, t2)        -> Either (t1, t2) (t1, t2)
    isFstZero           a@(0, _)        =  Left  a
    isFstZero           a               =  Right a
    -- -------------------------------------------------------------------------
    isFstOne         :: forall t1 t2. (Num t1, Eq t1) =>
                        (t1, t2)        -> Either (t1, t2) (t1, t2)
    isFstOne            a@(1, _)        =  Left  a
    isFstOne            a               =  Right a
