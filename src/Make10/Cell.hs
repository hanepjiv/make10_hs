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
                   , eval
                   , hasZeroDiv
                   , optimize
                   , expand
                   ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Applicative      ( (<$>)
                                , (<*>)
                                )

import qualified Make10.Operator as Op
import qualified Make10.Expand as Exp
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
-- 1 % 1
--
-- >>> Triple Op.ADD (Atom (1 % 1)) (Atom 2)
-- 1 % 1 + 2 % 1
--
-- >>> :{
--  Triple Op.ADD
--  (Atom 1) (Triple Op.ADD (Atom $ 1 % 2) (Atom $ 2 % 3))
-- :}
-- 1 % 1 + (1 % 2 + 2 % 3)
--
data Cell a where { Atom        :: a -> Cell a
                  ; Triple      :: !Op.Operator -> Cell a -> Cell a -> Cell a
                  } deriving (Eq)
-- -----------------------------------------------------------------------------
instance (Show a) => Show (Cell a) where
    showsPrec d (Atom x)        = showsPrec (succ d) x
    showsPrec d (Triple o l r)  =
      showParen (d > 0) $
      showsPrec (succ d) l . showsPrec (succ d) o . showsPrec (succ d) r
-- -----------------------------------------------------------------------------
-- | setOp
--
-- >>> setOp Op.ADD (Atom (1 % 1))
-- 1 % 1
--
-- >>> setOp Op.MUL (Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1)))
-- 1 % 1 * 2 % 1
--
setOp :: Op.Operator      -> Cell a               -> Cell a
setOp    op                  (Triple _ l r)       =  Triple op l r
setOp    _                   a                    =  a
-- -----------------------------------------------------------------------------
-- | setRightOp
--
-- >>> setRightOp Op.ADD (Atom (1 % 1))
-- 1 % 1
--
-- >>> :{
--  setRightOp Op.MUL
--             (Triple Op.ADD
--                     (Atom (1 % 1))
--                     (Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1))))
-- :}
-- 1 % 1 + (1 % 1 * 2 % 1)
--
setRightOp :: Op.Operator -> Cell a           -> Cell a
setRightOp    rop            (Triple op l r)  = Triple op l $ setOp rop r
setRightOp    _              a                =  a
-- -----------------------------------------------------------------------------
-- | apply
--
-- >>> apply Op.ADD (Atom (1 % 1)) (Atom 2)
-- Right (3 % 1)
--
-- >>> apply Op.SUB (Atom (1 % 1)) (Atom 2)
-- Right ((-1) % 1)
--
-- >>> apply Op.RSUB (Atom (1 % 1)) (Atom 2)
-- Right (1 % 1)
--
-- >>> apply Op.MUL (Triple Op.ADD (Atom (1 % 1)) (Atom 2)) (Atom 2)
-- Right (6 % 1)
--
-- >>> apply Op.DIV (Atom (1 % 1)) (Atom 2)
-- Right (1 % 2)
--
-- >>> apply Op.RDIV (Atom (1 % 1)) (Atom 2)
-- Right (2 % 1)
--
-- >>> :{
--  apply Op.MUL
--        (Triple Op.ADD (Atom (1 % 1)) (Atom 2))
--        (Triple Op.DIV (Atom 10) (Atom 2))
-- :}
-- Right (15 % 1)
--
-- >>> :{
--  apply Op.ADD (Atom $ 9 % 1)
--               (Triple Op.RDIV (Triple Op.MUL (Atom $ 3 % 1)
--                                              (Atom $ 9 % 1))
--                               (Atom $ 3 % 1))
-- :}
-- Right (82 % 9)
--
apply :: forall a.
         (Show a, Fractional a, Eq a) =>
         Op.Operator -> Cell a -> Cell a -> Either String a
apply op@Op.RDIV l r = apply (Op.swap op) r l
apply op@Op.DIV  l r =
  case eval r of
    l_@(Left  _) -> l_
    r_@(Right x) -> if 0 == x
                    then Left $ "ERROR!: apply: zero divide: " ++ show r
                    else Op.function op <$> eval l <*> r_
apply op l r = Op.function op <$> eval l <*> eval r
-- -----------------------------------------------------------------------------
-- | eval
--
eval :: forall a.
        (Show a, Fractional a, Eq a) =>
        Cell a -> Either String a
eval    (Atom x)        =  Right x
eval    (Triple op l r) =  apply op l r
-- -----------------------------------------------------------------------------
-- | hasZeroDiv
--
hasZeroDiv :: forall a.
              (Show a, Fractional a, Eq a) =>
              Cell a -> Bool
hasZeroDiv (Triple Op.DIV  _        (Atom 0))   = True
hasZeroDiv (Triple Op.RDIV (Atom 0) _)          = True
hasZeroDiv (Triple _       l        r)          = hasZeroDiv l || hasZeroDiv r
hasZeroDiv _                                    = False
-- -----------------------------------------------------------------------------
-- | rank
--
rank :: (Fractional a) =>
        Cell a          -> a
rank    (Atom x)        =  x
rank    (Triple _ l r)  =  10 + (rank l + rank r)
-- -----------------------------------------------------------------------------
-- | swap
--
-- >>> swap $ Atom (1 % 1)
-- *** Exception: Prelude.undefined
--
-- >>> swap $ Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1))
-- 2 % 1 + 1 % 1
--
swap ::         Cell a          -> Cell a
swap            (Triple op l r) =  Triple (Op.swap op) r l
swap            _               =  undefined
-- -----------------------------------------------------------------------------
-- | swapUnsafe
--
-- >>> swapUnsafe $ Atom (1 % 1)
-- *** Exception: Prelude.undefined
--
-- >>> swapUnsafe $ Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1))
-- 2 % 1 + 1 % 1
--
swapUnsafe ::   Cell a          -> Cell a
swapUnsafe      (Triple op l r) =  Triple op r l
swapUnsafe      _               =  undefined
-- -----------------------------------------------------------------------------
{-
-- UNUSED
-- | leftUnsafe
--
-- >>> leftUnsafe $ Atom (1 % 1)
-- *** Exception: Prelude.undefined
--
-- >>> :{
--  leftUnsafe $ Triple Op.MUL
--                (Atom (1 % 1))
--                (Triple Op.MUL (Atom (2 % 1)) (Atom (3 % 1)))
-- :}
-- Triple MUL (Triple MUL (Atom (1 % 1)) (Atom (2 % 1))) (Atom (3 % 1))
--
leftUnsafe :: Cell a                            -> Cell a
leftUnsafe    (Triple op l (Triple rop rl rr))  =
  Triple rop (Triple op l rl) rr
leftUnsafe    _                                 =  undefined
-- -}
-- -----------------------------------------------------------------------------
-- | rightUnsafe
--
-- >>> rightUnsafe $ Atom (1 % 1)
-- *** Exception: Prelude.undefined
--
-- >>> :{
--  rightUnsafe $ Triple Op.MUL
--                 (Triple Op.MUL (Atom (1 % 1)) (Atom (2 % 1)))
--                 (Atom (3 % 1))
-- :}
-- 1 % 1 * (2 % 1 * 3 % 1)
--
rightUnsafe :: Cell a                        -> Cell a
rightUnsafe (Triple op (Triple lop ll lr) r) =  Triple lop ll (Triple op lr r)
rightUnsafe _                                =  undefined
-- -----------------------------------------------------------------------------
-- | optimize
--
-- >>> optimize $ Triple Op.RSUB (Atom (1 % 1)) (Atom (2 % 1))
-- 2 % 1 - 1 % 1
--
-- >>> optimize $ Triple Op.RDIV (Atom (1 % 1)) (Atom (2 % 1))
-- 2 % 1 / 1 % 1
--
-- >>> :{
--  optimize $ Triple Op.ADD (Atom (9 % 1))
--                           (Triple Op.RDIV (Triple Op.RDIV (Atom (3 % 1))
--                                                           (Atom (9 % 1)))
--                                           (Atom (3 % 1)))
-- :}
-- 9 % 1 + (3 % 1 * (3 % 1 / 9 % 1))
--
optimize :: forall a.
            (Show a, Ord a, Fractional a) =>
            Cell a -> Cell a
optimize    x@(Atom {})         =  x
optimize    (Triple op l r)     =  opt (Triple op (optimize l) (optimize r))
  where
    -- -------------------------------------------------------------------------
    opt            x_@(Atom {})                      = x_
    opt            x_@(Triple Op.ADD  _ _)           = opt_ADD $ opt_rankSwap x_
    opt            x_@(Triple Op.SUB  _ _)           = opt_SUB x_
    opt            x_@(Triple Op.RSUB _ _)           = opt $ swap x_
    opt            x_@(Triple Op.MUL  _ _)           = opt_MUL $ opt_rankSwap x_
    opt            x_@(Triple Op.DIV  _ _)           = opt_DIV x_
    opt            x_@(Triple Op.RDIV _ _)           = opt $ swap x_
    -- -------------------------------------------------------------------------
    opt_rankSwap   x_@(Triple _ l_ r_)
      | rank l_ > rank r_                            = opt $ swapUnsafe x_
      | otherwise                                    = x_
    opt_rankSwap   x_                                = x_
    -- -------------------------------------------------------------------------
    opt_ADD x_@(Triple Op.ADD (Triple Op.ADD _ _) _) = optimize $ rightUnsafe x_
    opt_ADD x_@(Triple Op.ADD _ (Triple Op.ADD _ _)) = opt_change x_
    opt_ADD x_                                       = x_
    -- -------------------------------------------------------------------------
    opt_SUB x_@(Triple Op.SUB (Triple Op.SUB _ _) _) = opt_rightSafe x_
    opt_SUB x_@(Triple Op.SUB _ (Triple Op.SUB _ _)) = opt_invertSafe x_
    opt_SUB x_                                       = x_
    -- -------------------------------------------------------------------------
    opt_MUL x_@(Triple Op.MUL (Triple Op.MUL _ _) _) = optimize $ rightUnsafe x_
    opt_MUL x_@(Triple Op.MUL _ (Triple Op.MUL _ _)) = opt_change x_
    opt_MUL x_                                       = x_
    -- -------------------------------------------------------------------------
    opt_DIV x_@(Triple Op.DIV (Triple Op.DIV _ _) _) = opt_rightSafe x_
    opt_DIV x_@(Triple Op.DIV _ (Triple Op.DIV _ _)) = opt_invertSafe x_
{-
    opt_DIV x_@(Triple Op.DIV _ r_)                  =
      case eval r_ of
        Right x -> if 1 == x
                   then                                opt $ setOp Op.MUL x_
                   else                                x_
        _       -> x_
-}
    opt_DIV        x_                                = x_
    -- -------------------------------------------------------------------------
    opt_rightSafe  x_@(Triple _ (Triple rop_ _ _) _) =
      optimize $ setRightOp (Op.invert rop_) $ rightUnsafe x_
    opt_rightSafe  x_                                = x_
    -- -------------------------------------------------------------------------
    opt_invertSafe    (Triple op_ l_ r_@(Triple {})) =
      optimize $ Triple (Op.invert op_) l_ $ swapUnsafe r_
    opt_invertSafe x_                                = x_
    -- -------------------------------------------------------------------------
    opt_change     x_@(Triple op_ l_ (Triple rop_ rl_ rr_))
      | rank l_ > rank rl_ = Triple op_ rl_ $ opt $ Triple rop_ l_ rr_
      | otherwise                                    = x_
    opt_change     x_                                = x_
-- -----------------------------------------------------------------------------
-- | expand
--
-- >>> expand $ (Atom $ 1 % 1)
-- ExpandList [1 % 1]
--
-- >>> expand $ Triple Op.RSUB (Atom $ 1 % 1) (Atom $ 2 % 1)
-- ExpandList [(-1) % 1,2 % 1]
--
-- >>> expand $ Triple Op.DIV (Atom $ 10 % 1) (Atom $ 10 % 1)
-- ExpandTuple ([10 % 1],[10 % 1])
--
expand :: forall a. (Ord a, Num a) => Cell a -> Exp.Expand a
expand (Atom 0)                 =  Exp.ExpandList []
expand (Atom x)                 =  Exp.ExpandList [x]
expand (Triple op lhs rhs)      =  expandFunc op (expand lhs) (expand rhs)
  where
    expandFunc Op.ADD  =      Exp.add
    expandFunc Op.SUB  =      Exp.sub
    expandFunc Op.RSUB = flip Exp.sub
    expandFunc Op.MUL  =      Exp.mul
    expandFunc Op.DIV  =      Exp.truediv
    expandFunc Op.RDIV = flip Exp.truediv
