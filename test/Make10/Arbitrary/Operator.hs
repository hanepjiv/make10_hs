{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.Arbitrary.Operator
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.Arbitrary.Operator(Operator(..)) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Applicative      ( (<$>)
                                )

import Test.QuickCheck

import qualified Make10
-- =============================================================================
-- -----------------------------------------------------------------------------
newtype Operator = Operator { getBase :: Make10.Operator }
                   deriving (Show)
-- -----------------------------------------------------------------------------
instance Arbitrary Operator where
  arbitrary =  gen <$> arbitrary
    where
      gen :: Int -> Operator
      gen i = Operator $
              toEnum $ mod (abs i) (fromEnum (maxBound :: Make10.Operator) + 1)
