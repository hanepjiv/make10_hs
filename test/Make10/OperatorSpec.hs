{-# LANGUAGE OverloadedStrings #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.OperatorSpec
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.OperatorSpec(spec) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Test.Hspec
import Test.QuickCheck

import Make10
-- =============================================================================
-- -----------------------------------------------------------------------------
spec :: Spec
spec =  do
  describe "Operator" $ do
    it "ADD" $ property $ \ x y -> function ADD (x :: Rational) y == x + y
    it "SUB" $ property $ \ x y -> function SUB (x :: Rational) y == x - y
    it "MUL" $ property $ \ x y -> function MUL (x :: Rational) y == x * y
    it "DIV" $ property $ \ x y -> case y of
      0 -> True
      _ -> function DIV (x :: Rational) y == x / y
  describe "apply" $ do
    it "apply op Atom Atom" $ property $
      \ m n op -> apply op (Atom (m :: Rational)) (Atom n) == function op m n
    it "apply op Triple Atom" $ property $
      \ m n o ->
      apply ADD (Triple SUB (Atom (m :: Rational)) (Atom n)) (Atom o) ==
      ((m - n) + o) &&
      apply SUB (Triple SUB (Atom (m :: Rational)) (Atom n)) (Atom o) ==
      ((m - n) - o) &&
      apply MUL (Triple SUB (Atom (m :: Rational)) (Atom n)) (Atom o) ==
      ((m - n) * o)
    it "apply op Atom Triple" $ property $
      \ m n o ->
      apply ADD (Atom o) (Triple SUB (Atom (m :: Rational)) (Atom n)) ==
      (o + (m - n)) &&
      apply SUB (Atom o) (Triple SUB (Atom (m :: Rational)) (Atom n)) ==
      (o - (m - n)) &&
      apply MUL (Atom o) (Triple SUB (Atom (m :: Rational)) (Atom n)) ==
      (o * (m - n))
    it "apply op Triple Triple" $ property $
      \ m n o p ->
      apply ADD (Triple SUB (Atom (m :: Rational)) (Atom n))
      (Triple SUB (Atom o) (Atom p)) == ((m - n) + (o - p)) &&
      apply SUB (Triple SUB (Atom (m :: Rational)) (Atom n))
      (Triple SUB (Atom o) (Atom p)) == ((m - n) - (o - p)) &&
      apply MUL (Triple SUB (Atom (m :: Rational)) (Atom n))
      (Triple SUB (Atom o) (Atom p)) == ((m - n) * (o - p))
