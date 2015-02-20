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
              , eval
              , optimize
              , expand
                -- -------------------------------------------------------------
              , make_M_4
              ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Applicative
import Control.Monad    ( replicateM
                        )

import qualified Data.Set as Set

import Make10.Operator
import Make10.Cell
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings

-- =============================================================================
-- -----------------------------------------------------------------------------
-- | make_M_4_Triple_A
--
make_M_4_Triple_A :: forall a.
               (Show a, Ord a, Fractional a) =>
               a -> a -> a -> a -> Operator -> Operator -> Operator -> Cell a
make_M_4_Triple_A n0 n1 n2 n3 o0 o1 o2 =
  Triple o2 (Triple o1 (Triple o0 (Atom n3) (Atom n2)) (Atom n1)) (Atom n0)
-- -----------------------------------------------------------------------------
-- | make_M_4_Triple_B
--
make_M_4_Triple_B :: forall a.
               (Show a, Ord a, Fractional a) =>
               a -> a -> a -> a -> Operator -> Operator -> Operator -> Cell a
make_M_4_Triple_B n0 n1 n2 n3 o0 o1 o2 =
  Triple o2 (Triple o1 (Atom n3) (Atom n2)) (Triple o0 (Atom n1) (Atom n0))
-- -----------------------------------------------------------------------------
-- | patternA
--
patternA :: [[Integer]]
patternA =  [ [0, 1, 2, 3]
            , [0, 2, 1, 3]
            , [0, 3, 1, 2]
            , [1, 0, 2, 3]
            , [1, 2, 0, 3]
            , [1, 3, 0, 2]
            , [2, 0, 1, 3]
            , [2, 1, 0, 3]
            , [2, 3, 0, 1]
            , [3, 0, 1, 2]
            , [3, 1, 0, 2]
            , [3, 2, 0, 1]
            ]
-- -----------------------------------------------------------------------------
-- | patternB
--
patternB :: [[Integer]]
patternB  = [ [0, 1, 2, 3]
            , [0, 2, 1, 3]
            , [0, 3, 1, 2]
            ]
-- -----------------------------------------------------------------------------
-- | make_M_4_Triple
--
make_M_4_Triple :: forall a.
              (Show a, Ord a, Fractional a) =>
              [a] -> [Operator] -> [Cell a]
make_M_4_Triple ns os =
  filter (not . hasZeroDiv) (map (gen make_M_4_Triple_A ns os) patternA ++
                             map (gen make_M_4_Triple_B ns os) patternB)
  where
    gen make_ n_ o_ i_ = make_
                         (n_ !! fromInteger (head i_))
                         (n_ !! fromInteger (i_ !! 1))
                         (n_ !! fromInteger (i_ !! 2))
                         (n_ !! fromInteger (i_ !! 3))
                         (head o_)
                         (o_ !! 1)
                         (o_ !! 2)
-- -----------------------------------------------------------------------------
-- | make_M_4
--
make_M_4 :: forall a.
         (Show a, Ord a, Fractional a) =>
         a -> [a] -> [Cell a]
make_M_4 n a_in =
  unseen [optimize t | t <- concatMap (make_M_4_Triple a_in) $
                            replicateM (pred (length a_in)) allOp
                     , isRightTrue $ (== n) <$> eval t
                     ]
  where
    -- -------------------------------------------------------------------------
    isRightTrue (Right True)       = True
    isRightTrue _                  = False
    -- -------------------------------------------------------------------------
    unseen [] = []
    unseen x_ = unseen_ x_ Set.empty
      where
        unseen_   []     _      =  []
        unseen_   (x:xs) seen   = unseen__ x xs seen $! expand x
          where
            unseen__ x__ xs__ seen_ e
              | e `Set.member` seen_ =  unseen_ xs__ seen_
              | otherwise            =  x__ : unseen_ xs__ (Set.insert e seen_)
