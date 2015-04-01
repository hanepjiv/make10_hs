{-# LANGUAGE      ScopedTypeVariables
                , OverloadedStrings
                , GADTs
                #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.OperatorSpec
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
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
spec =  --do
  describe "Operator" $ do
    it "ADD" $ property $ \ x y -> function ADD (x :: Rational) y == x + y
    it "SUB" $ property $ \ x y -> function SUB (x :: Rational) y == x - y
    it "MUL" $ property $ \ x y -> function MUL (x :: Rational) y == x * y
    it "DIV" $ property $ \ x y -> case y of
      0 -> True
      _ -> function DIV (x :: Rational) y == x / y
