{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.Operator
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.Operator ( Operator(..)
                       , invert
                       , swap
                       , isEnableSwap
                       , priority
                       , function
                       ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Applicative( (<$>)
                          )

import qualified Test.QuickCheck as QuickCheck
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Ratio((%), Ratio, Rational)

-- =============================================================================
-- -----------------------------------------------------------------------------
-- | Operator
--
data Operator   = ADD
                | SUB
                | RSUB
                | MUL
                | DIV
                | RDIV
                deriving (Bounded, Eq, Ord, Enum, Show)
-- -----------------------------------------------------------------------------
instance QuickCheck.Arbitrary Operator where
  arbitrary =  gen <$> QuickCheck.arbitrary
    where
      gen :: Int -> Operator
      gen i = toEnum $ mod (abs i) (fromEnum (maxBound :: Operator) + 1)
-- -----------------------------------------------------------------------------
-- | invert
--
invert :: Operator      -> Operator
invert ADD              =  SUB
invert SUB              =  ADD
invert RSUB             =  undefined
invert MUL              =  DIV
invert DIV              =  MUL
invert RDIV             =  undefined
-- -----------------------------------------------------------------------------
-- | swap
--
swap :: Operator        -> Operator
swap ADD                =  ADD
swap SUB                =  RSUB
swap RSUB               =  SUB
swap MUL                =  MUL
swap DIV                =  RDIV
swap RDIV               =  DIV
-- -----------------------------------------------------------------------------
-- | isEnableSwap
--
isEnableSwap :: Operator -> Bool
isEnableSwap op = op == swap op
-- -----------------------------------------------------------------------------
-- | priority
--
priority :: Operator    -> Int
priority ADD            =  minBound :: Int
priority SUB            =  minBound :: Int
priority RSUB           =  minBound :: Int
priority MUL            =  maxBound :: Int
priority DIV            =  maxBound :: Int
priority RDIV           =  maxBound :: Int
-- -----------------------------------------------------------------------------
-- | function
--
-- >>> function ADD (1 % 1) 2
-- 3 % 1
--
-- >>> function SUB (1 % 1) 2
-- (-1) % 1
--
-- >>> function MUL (1 % 1) 2
-- 2 % 1
--
-- >>> function DIV (1 % 1) 2
-- 1 % 2
--
function :: (Fractional a) => Operator -> a -> a -> a
function ADD            =  (+)
function RSUB           =  flip (-)
function SUB            =  (-)
function MUL            =  (*)
function DIV            =  (/)
function RDIV           =  flip (/)
