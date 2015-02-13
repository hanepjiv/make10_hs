{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.CellSpec
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.CellSpec(spec) where
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
  describe "apply" $ do
    it "apply op Atom Atom" $ property $
      \ m n op ->
      case apply op (Atom (m :: Rational)) (Atom n) of
        Right x -> x == function op m n
        _       -> True
    it "apply op Triple Atom" $ property $
      \ m n o ->
      case apply ADD (Triple SUB (Atom (m :: Rational)) (Atom n)) (Atom o) of
        Right x -> x == ((m - n) + o)
        _       -> True
      &&
      case apply SUB (Triple SUB (Atom (m :: Rational)) (Atom n)) (Atom o) of
        Right x -> x == ((m - n) - o)
        _       -> True
      &&
      case apply MUL (Triple SUB (Atom (m :: Rational)) (Atom n)) (Atom o) of
        Right x -> x == ((m - n) * o)
        _       -> True
    it "apply op Atom Triple" $ property $
      \ m n o ->
      case apply ADD (Atom o) (Triple SUB (Atom (m :: Rational)) (Atom n)) of
        Right x -> x == (o + (m - n))
        _       -> True
      &&
      case apply SUB (Atom o) (Triple SUB (Atom (m :: Rational)) (Atom n)) of
        Right x -> x == (o - (m - n))
        _       -> True
      &&
      case apply MUL (Atom o) (Triple SUB (Atom (m :: Rational)) (Atom n)) of
        Right x -> x == (o * (m - n))
        _       -> True
    it "apply op Triple Triple" $ property $
      \ m n o p ->
      case apply
           ADD
           (Triple SUB (Atom (m :: Rational)) (Atom n))
           (Triple SUB (Atom o) (Atom p)) of
        Right x -> x == ((m - n) + (o - p))
        _       -> True
      &&
      case apply
           SUB
           (Triple SUB (Atom (m :: Rational)) (Atom n))
           (Triple SUB (Atom o) (Atom p)) of
        Right x -> x == ((m - n) - (o - p))
        _       -> True
      &&
      case apply
           MUL
           (Triple SUB (Atom (m :: Rational)) (Atom n))
           (Triple SUB (Atom o) (Atom p)) of
        Right x -> x == ((m - n) * (o - p))
        _       -> True
