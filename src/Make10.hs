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
              , makeN
              , make10
              ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Applicative
import Control.Monad    ( replicateM
                        )

import Make10.Operator
import Make10.Cell
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings

-- =============================================================================
-- -----------------------------------------------------------------------------
-- | makeTripleA
--
makeTripleA :: forall a.
               (Show a, Ord a, Fractional a) =>
               a -> a -> a -> a -> Operator -> Operator -> Operator -> Cell a
makeTripleA n0 n1 n2 n3 o0 o1 o2 =
  Triple o2 (Triple o1 (Triple o0 (Atom n3) (Atom n2)) (Atom n1)) (Atom n0)
-- -----------------------------------------------------------------------------
-- | makeTripleB
--
makeTripleB :: forall a.
               (Show a, Ord a, Fractional a) =>
               a -> a -> a -> a -> Operator -> Operator -> Operator -> Cell a
makeTripleB n0 n1 n2 n3 o0 o1 o2 =
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
-- | makeTriple
--
makeTriple :: forall a.
              (Show a, Ord a, Fractional a) =>
              [a] -> [Operator] -> [Cell a]
makeTriple ns os =
  filter (not . hasZeroDiv)
  $ map (gen makeTripleA ns os) patternA ++ map (gen makeTripleB ns os) patternB
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
-- | makeN
--
makeN :: forall a.
         (Show a, Ord a, Fractional a) =>
         a -> [a] -> [Cell a]
makeN n a_in =
  unseen [optimize t | t <- concatMap (makeTriple a_in) $
                            replicateM (pred (length a_in)) allOp
                     , isRightTrue $ (== n) <$> eval t
                     ]
  where
    -- -------------------------------------------------------------------------
    isRightTrue (Right True)       = True
    isRightTrue _                  = False
    -- -------------------------------------------------------------------------
    unseen x_ = unseen_ x_ []
      where
        unseen_   []     _         = []
        unseen_ a@[_]    []        = a
        unseen_   (x:xs) seen
          | expand x `elem` seen   = unseen_ xs seen
          | otherwise              = x : unseen_ xs (expand x:seen)
-- -----------------------------------------------------------------------------
-- | make10
--
make10 :: forall a.
          (Show a, Ord a, Fractional a) =>
          [a] -> [Cell a]
make10 = makeN 10
