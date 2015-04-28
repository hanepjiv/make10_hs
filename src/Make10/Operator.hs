{-# LANGUAGE      ScopedTypeVariables
                , OverloadedStrings
                , GADTs
                #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.Operator
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.Operator ( Operator(..)
                       , allOp
                       , invert
                       , swap
                       , function
                       ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude
-- (Show, show, Bounded, minBound, maxBound, Eq, Ord, Enum, undefined,
-- Fractional, (+), (-), flip, (*), (/))
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
                deriving (Bounded, Eq, Ord, Enum)
-- -----------------------------------------------------------------------------
instance Show Operator where
    show ADD       = " + "
    show SUB       = " - "
    show RSUB      = " -< "
    show MUL       = " * "
    show DIV       = " / "
    show RDIV      = " /< "
-- -----------------------------------------------------------------------------
-- | allOp
--
allOp :: [Operator]
allOp =  [minBound .. maxBound]
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
-- | function
--
-- >>> function ADD (1 % 1) 2
-- 3 % 1
--
-- >>> function SUB (1 % 1) 2
-- (-1) % 1
--
-- >>> function RSUB (1 % 1) 2
-- 1 % 1
--
-- >>> function MUL (1 % 1) 2
-- 2 % 1
--
-- >>> function DIV (1 % 1) 2
-- 1 % 2
--
-- >>> function RDIV (1 % 1) 2
-- 2 % 1
--
function :: (Fractional a) => Operator -> a -> a -> a
function ADD            =  (+)
function SUB            =  (-)
function RSUB           =  flip (-)
function MUL            =  (*)
function DIV            =  (/)
function RDIV           =  flip (/)
