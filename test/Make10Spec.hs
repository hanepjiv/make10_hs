{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10Spec
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10Spec(spec) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Test.Hspec
--import Test.QuickCheck
-- =============================================================================
-- -----------------------------------------------------------------------------
spec :: Spec
spec = return ()
{--
  describe "select" $
    it "select" $ property $
      \ xs -> length (xs :: [Int]) == length (select xs)
--}
