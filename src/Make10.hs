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
--select (x:xs)   = (x,xs) : map (\ (y,ys) -> (y,x:ys)) (select xs)
select (x:xs)   = (x,xs) : map (second ((:) x))       (select xs)
-}
select =  loop $ (snd &&& fst >>> app) &&&
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
combinations :: (Num a, Eq a, Enum a) => a -> [a1] -> [[a1]]
combinations _ []     = []
combinations 1 x      = map (:[]) x
combinations n (x:xs) =
  (map (x:) (combinations (pred n) xs)) ++ (combinations n xs)



comb2 :: (Num a, Eq a, Enum a) => (a, [a1]) -> [[a1]]
comb2 =  loop $ (snd &&& fst >>> app) &&&
         ((<<< isSndNull)
          <<< ((const []) |||)
          <<< (<<< isOne)
          <<< (((map (:[])) <<< snd) |||)

          <<< (uncurry map <<<)
          <<< ((uncurry (:) <<< head <<< snd) ***
               (((pred <<< fst) &&& (tail <<< snd)) ***
                (fst &&& (tail <<< snd)))
               <<< (id &&& id) <<<)
          <<< ((id &&& id) <<<)

          <<< snd)
  where
    isSndNull (_, [])   = Left  []
    isSndNull a@(_, _)  = Right a
    isOne a@(1, _)      = Left  a
    isOne a             = Right a



{-
          <<< ((((uncurry map <<<)
                 <<< ((uncurry (:) <<< head <<< snd)
                      &&& (<<< ((pred <<< fst) &&& (tail <<< snd))))))
               &&& (<<< (fst &&& (tail <<< snd))))
-}
