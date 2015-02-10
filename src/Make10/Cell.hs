{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.Cell
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.Cell ( Cell(..)
                   , apply
                   , swap
                   ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import qualified Make10.Operator as Operator
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Ratio((%), Ratio, Rational)

-- =============================================================================
-- -----------------------------------------------------------------------------
-- | Cell
--
-- >>> Atom (1 % 1)
-- Atom (1 % 1)
--
-- >>> Triple Operator.ADD (Atom (1 % 1)) (Atom 2)
-- Triple ADD (Atom (1 % 1)) (Atom (2 % 1))
--
-- >>> :{
-- Triple Operator.ADD
-- (Atom 1) (Triple Operator.ADD (Atom $ 1 % 2) (Atom $ 2 % 3))
-- :}
-- Triple ADD (Atom (1 % 1)) (Triple ADD (Atom (1 % 2)) (Atom (2 % 3)))
data Cell a where
  { Atom        :: a -> Cell a
  ; Triple      :: !Operator.Operator -> Cell a -> Cell a -> Cell a
  } deriving (Eq, Show)
-- -----------------------------------------------------------------------------
-- | apply
--
-- >>> apply Operator.ADD (Atom (1 % 1)) (Atom 2)
-- 3 % 1
--
-- >>> apply Operator.MUL (Triple Operator.ADD (Atom (1 % 1)) (Atom 2)) (Atom 2)
-- 6 % 1
--
-- >>> :{
--  apply Operator.MUL
--        (Triple Operator.ADD (Atom (1 % 1)) (Atom 2))
--        (Triple Operator.DIV (Atom 10) (Atom 2))
-- :}
-- 15 % 1
apply :: (Fractional a) => Operator.Operator -> Cell a -> Cell a -> a
apply op (Atom l) (Atom r) = Operator.function op l r
apply op (Triple lop ll lr) a_rhs = apply op (Atom (apply lop ll lr)) a_rhs
apply op a_lhs@(Atom {}) a_rhs@(Triple {}) =
  apply (Operator.swap op) a_rhs a_lhs
-- -----------------------------------------------------------------------------
-- | swap
--
swap :: Cell a -> Cell a
swap (Atom _)           = undefined
swap (Triple op l r)    = Triple op r l
