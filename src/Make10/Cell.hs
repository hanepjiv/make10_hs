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
                   , optimize
                   , expand
                   ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Data.List

import Control.Applicative ( (<$>)
                           )

import qualified Make10.Operator as Op
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
-- >>> Triple Op.ADD (Atom (1 % 1)) (Atom 2)
-- Triple ADD (Atom (1 % 1)) (Atom (2 % 1))
--
-- >>> :{
--  Triple Op.ADD
--  (Atom 1) (Triple Op.ADD (Atom $ 1 % 2) (Atom $ 2 % 3))
-- :}
-- Triple ADD (Atom (1 % 1)) (Triple ADD (Atom (1 % 2)) (Atom (2 % 3)))
--
data Cell a where
  { Atom        :: a -> Cell a
  ; Triple      :: !Op.Operator -> Cell a -> Cell a -> Cell a
  } deriving (Eq, Show)
-- -----------------------------------------------------------------------------
-- | setOp
--
-- >>> setOp Op.ADD (Atom (1 % 1))
-- Atom (1 % 1)
--
-- >>> setOp Op.MUL (Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1)))
-- Triple MUL (Atom (1 % 1)) (Atom (2 % 1))
--
setOp :: Op.Operator      -> Cell a               -> Cell a
setOp    op                  (Triple _ l r)       =  Triple op l r
setOp    _                   a                    =  a
-- -----------------------------------------------------------------------------
-- | setRightOp
--
-- >>> setRightOp Op.ADD (Atom (1 % 1))
-- Atom (1 % 1)
--
-- >>> :{
--  setRightOp Op.MUL
--             (Triple Op.ADD
--                     (Atom (1 % 1))
--                     (Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1))))
-- :}
-- Triple ADD (Atom (1 % 1)) (Triple MUL (Atom (1 % 1)) (Atom (2 % 1)))
--
setRightOp :: Op.Operator -> Cell a           -> Cell a
setRightOp    rop            (Triple op l r)  = Triple op l $ setOp rop r
setRightOp    _              a                =  a
-- -----------------------------------------------------------------------------
-- | apply
--
-- >>> apply Op.ADD (Atom (1 % 1)) (Atom 2)
-- 3 % 1
--
-- >>> apply Op.MUL (Triple Op.ADD (Atom (1 % 1)) (Atom 2)) (Atom 2)
-- 6 % 1
--
-- >>> :{
--  apply Op.MUL
--        (Triple Op.ADD (Atom (1 % 1)) (Atom 2))
--        (Triple Op.DIV (Atom 10) (Atom 2))
-- :}
-- 15 % 1
--
apply :: (Fractional a) => Op.Operator -> Cell a -> Cell a -> a
apply op (Atom l) (Atom r) = Op.function op l r
apply op (Triple lop ll lr) a_rhs = apply op (Atom (apply lop ll lr)) a_rhs
apply op a_lhs@(Atom {}) a_rhs@(Triple {}) =
  apply (Op.swap op) a_rhs a_lhs
-- -----------------------------------------------------------------------------
-- | eval
--
eval :: (Fractional a) =>
        Cell a          -> a
eval    (Atom x)        =  x
eval    (Triple op l r) =  apply op l r
-- -----------------------------------------------------------------------------
-- | rank
--
rank :: (Fractional a) =>
        Cell a          -> a
rank    (Atom x)        =  x
rank    (Triple _ l r)  =  10 * (rank l + rank r)
-- -----------------------------------------------------------------------------
-- | swap
--
-- >>> swap $ Atom (1 % 1)
-- *** Exception: Prelude.undefined
--
-- >>> swap $ Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1))
-- Triple ADD (Atom (2 % 1)) (Atom (1 % 1))
--
swap :: Cell a                          -> Cell a
swap    (Triple op l r)                 = Triple (Op.swap op) r l
swap    _                               = undefined
-- -----------------------------------------------------------------------------
-- | swapUnsafe
--
-- >>> swapUnsafe $ Atom (1 % 1)
-- *** Exception: Prelude.undefined
--
-- >>> swapUnsafe $ Triple Op.ADD (Atom (1 % 1)) (Atom (2 % 1))
-- Triple ADD (Atom (2 % 1)) (Atom (1 % 1))
--
swapUnsafe      :: Cell a               -> Cell a
swapUnsafe         (Triple op l r)      = Triple op r l
swapUnsafe         _                    = undefined
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
-- Triple MUL (Atom (1 % 1)) (Triple MUL (Atom (2 % 1)) (Atom (3 % 1)))
--
rightUnsafe:: Cell a                            -> Cell a
rightUnsafe   (Triple op (Triple lop ll lr) r)  =
  Triple lop ll (Triple op lr r)
rightUnsafe   _                                 =  undefined
-- -----------------------------------------------------------------------------
-- | optimize
--
-- >>> optimize $ Triple Op.RSUB (Atom (1 % 1)) (Atom (2 % 1))
-- Triple SUB (Atom (2 % 1)) (Atom (1 % 1))
--
-- >>> optimize $ Triple Op.RDIV (Atom (1 % 1)) (Atom (2 % 1))
-- Triple MUL (Atom (1 % 1)) (Atom (2 % 1))
--
optimize :: (Fractional a, Eq a, Ord a) =>
            Cell a              -> Cell a
optimize    x@(Atom {})         =  x
optimize    (Triple op l r)     =  opt (Triple op (optimize l) (optimize r))
  where
    -- -------------------------------------------------------------------------
    opt x_@(Triple Op.RSUB _ _)                  = opt $ swap x_
    opt x_@(Triple Op.RDIV _ _)                  = opt $ swap x_

    opt x_@(Triple Op.SUB _ (Triple Op.SUB _ _)) = opt_0 x_
    opt x_@(Triple Op.DIV _ (Triple Op.DIV _ _)) = opt_0 x_

    opt x_@(Triple Op.SUB (Triple Op.SUB _ _) _) = opt_1 x_
    opt x_@(Triple Op.DIV (Triple Op.DIV _ _) _) = opt_1 x_

    opt x_@(Triple Op.DIV _ r_)
      | 1 == eval r_                             = opt $ setOp Op.MUL x_
      | otherwise                                = x_

    opt x_@(Triple Op.ADD (Triple Op.ADD _ _) _) = optimize $ rightUnsafe x_
    opt x_@(Triple Op.MUL (Triple Op.MUL _ _) _) = optimize $ rightUnsafe x_

    opt x_@(Triple Op.ADD _ (Triple Op.ADD _ _)) = opt_2 x_
    opt x_@(Triple Op.MUL _ (Triple Op.MUL _ _)) = opt_2 x_

    opt x_@(Triple Op.ADD _ _)                   = opt_3 x_
    opt x_@(Triple Op.MUL _ _)                   = opt_3 x_

    opt x_                                       = x_
    -- -------------------------------------------------------------------------
    opt_0 (Triple op_ l_ r_@(Triple rop_ _ _))   =
      Triple op_ l_ $ opt $ setOp (Op.invert rop_) $ swapUnsafe r_
    opt_0 x_                                     = x_
    -- -------------------------------------------------------------------------
    opt_1 x_@(Triple op_ Triple{} _)             =
      optimize $ setRightOp (Op.invert op_) $ rightUnsafe x_
    opt_1 x_                                     = x_
    -- -------------------------------------------------------------------------
    opt_2 x_@(Triple op_ l_ (Triple rop_ rl_ rr_))
      | rank l_ > rank rl_ = Triple op_ rl_ $ opt $ Triple rop_ l_ rr_
      | otherwise                                = x_
    opt_2 x_                                     = x_
    -- -------------------------------------------------------------------------
    opt_3 x_@(Triple _ l_ r_)
      | rank l_ > rank r_                        = opt $ swapUnsafe x_
      | otherwise                                = x_
    opt_3 x_                                     = x_

-- -----------------------------------------------------------------------------
-- | expand
--
-- >>> expand $ (Atom $ 1 % 1)
-- [1 % 1]
--
-- >>> expand $ Triple Op.RSUB (Atom $ 1 % 1) (Atom $ 2 % 1)
-- [(-1) % 1,2 % 1]
--
-- >>> expand $ Triple Op.DIV (Atom $ 10 % 1) (Atom $ 10 % 1)
-- [1 % 1]
--
expand :: (Fractional t, Ord t) =>
          Cell t                                         -> [t]
expand    x                                              =  sort $ e_ x
  where
    e_    (Atom x_)                                      =  [x_]
    e_    (Triple Op.ADD  l_ r_)                         =  e_ l_ ++ e_ r_
    e_    (Triple Op.SUB  l_ r_) = e_ l_ ++ ((* (-1)) <$> e_ r_)
    e_ x_@(Triple Op.RSUB _  _)                          =  e_ $ optimize x_
    e_    (Triple Op.MUL     (Atom l_)      (Atom r_))   =  [l_ * r_]
    e_    (Triple Op.MUL     (Atom l_)   r_@(Triple {})) =  (* l_) <$> e_ r_
    e_    (Triple Op.MUL  l_@(Triple {})    (Atom r_))   =  (* r_) <$> e_ l_
    e_ x_@(Triple Op.MUL     (Triple {})    (Triple {})) =  e_ $ optimize x_
    e_    (Triple Op.DIV     (Atom l_)      (Atom r_))   =  [l_ / r_]
    e_    (Triple Op.DIV     (Atom l_)   r_@(Triple {})) =  (/ l_) <$> e_ r_
    e_    (Triple Op.DIV  l_@(Triple {})    (Atom r_))   =  (/ r_) <$> e_ l_
    e_ x_@(Triple Op.DIV     (Triple {})    (Triple {})) =  e_ $ optimize x_
    e_ x_@(Triple Op.RDIV _  _)                          =  e_ $ optimize x_
