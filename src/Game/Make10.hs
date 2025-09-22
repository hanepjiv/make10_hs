{-# LANGUAGE      ScopedTypeVariables
                , GADTs
                , Safe
                #-}
-- ============================================================================
-- ----------------------------------------------------------------------------
{-|
Module      : Game.Make10
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
License     : MIT
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Game.Make10              ( Operator(..)
                                , function
                                -- --------------------------------------------
                                , Cell(..)
                                , apply
                                , eval
                                , optimize
                                , expand
                                -- --------------------------------------------
                                , make_M_4
                                ) where
-- ============================================================================
-- ----------------------------------------------------------------------------
import Prelude

import Control.Monad            ( replicateM
                                )

import qualified Data.Set as Set

import Game.Make10.Operator
import Game.Make10.Cell
import Game.Make10.Expand       ( Expand(..)
                                )
-- ============================================================================
-- ----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
--

-- ============================================================================
-- ----------------------------------------------------------------------------
-- | make_M_4_Triple
--
make_M_4_Triple :: forall a.
              (Show a, Ord a, Fractional a) =>
              [a] -> [Operator] -> [Cell a]
make_M_4_Triple ns os =
  filter (not . hasZeroDiv) (map (gen make_M_4_Triple_A ns os) patternA ++
                             map (gen make_M_4_Triple_B ns os) patternB)
  where
    -- ------------------------------------------------------------------------
    gen :: forall t a0 a1.
           (a0 -> a0 -> a0 -> a0 -> a1 -> a1 -> a1 -> t)
               -> [a0] -> [a1] -> [Integer] -> t
    gen make_ n_ o_ i_ = make_
                         (n_ !! fromInteger (i_ !! 0))
                         (n_ !! fromInteger (i_ !! 1))
                         (n_ !! fromInteger (i_ !! 2))
                         (n_ !! fromInteger (i_ !! 3))
                         (o_ !! 0)
                         (o_ !! 1)
                         (o_ !! 2)
    -- ------------------------------------------------------------------------
    make_M_4_Triple_A :: forall a0.
                         a0 -> a0 -> a0 -> a0
                            -> Operator -> Operator -> Operator -> Cell a0
    make_M_4_Triple_A n0 n1 n2 n3 o0 o1 o2 =
      Triple o2 (Triple o1 (Triple o0 (Atom n3) (Atom n2)) (Atom n1)) (Atom n0)
    -- ------------------------------------------------------------------------
    make_M_4_Triple_B :: forall a0.
                         a0 -> a0 -> a0 -> a0
                            -> Operator -> Operator -> Operator -> Cell a0
    make_M_4_Triple_B n0 n1 n2 n3 o0 o1 o2 =
      Triple o2 (Triple o1 (Atom n3) (Atom n2)) (Triple o0 (Atom n1) (Atom n0))
    -- ------------------------------------------------------------------------
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
    -- ------------------------------------------------------------------------
    patternB :: [[Integer]]
    patternB  = [ [0, 1, 2, 3]
                , [0, 2, 1, 3]
                , [0, 3, 1, 2]
                ]
-- ----------------------------------------------------------------------------
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
    -- ------------------------------------------------------------------------
    isRightTrue :: forall t. Either t Bool -> Bool
    isRightTrue (Right True)       = True
    isRightTrue _                  = False
    -- ------------------------------------------------------------------------
    unseen :: forall a0. (Ord a0, Num a0) => [Cell a0] -> [Cell a0]
    unseen [] = []
    unseen x_ = unseen_ x_ Set.empty
      where
        unseen_ :: forall a1. (Ord a1, Num a1) =>
                   [Cell a1] -> Set.Set (Game.Make10.Expand.Expand a1)
                             -> [Cell a1]
        unseen_   []     _      =  []
        unseen_   (x:xs) seen   = unseen__ x xs seen $! expand x
          where
            unseen__ :: forall a2. (Ord a2, Num a2) =>
                        Cell a2 -> [Cell a2] -> Set.Set (Expand a2) ->
                                   Expand a2 -> [Cell a2]
            unseen__ x__ xs__ seen_ e
              | e `Set.member` seen_ =  unseen_ xs__ seen_
              | otherwise            =  x__ : unseen_ xs__ (Set.insert e seen_)
