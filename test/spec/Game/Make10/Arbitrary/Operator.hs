{-# LANGUAGE      ScopedTypeVariables
                , GADTs
                #-}
-- ============================================================================
-- ----------------------------------------------------------------------------
{-|
Module      : Make10.Arbitrary.Operator
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
License     : MIT
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Game.Make10.Arbitrary.Operator   ( Operator(..)
                                        ) where
-- ============================================================================
-- ----------------------------------------------------------------------------
import Prelude

import Test.QuickCheck

import qualified Game.Make10
-- ============================================================================
-- ----------------------------------------------------------------------------
{- 0.0 -- ---------------------------------------------------------------------
-- field
newtype Operator = Operator { getBase :: Game.Make10.Operator }
                 deriving (Show)
-- -}
{- 0.1 -- ---------------------------------------------------------------------
-- GADTs --}
newtype Operator where { Operator       :: Game.Make10.Operator
                                        -> Operator
                       } deriving ( Show
                                  )
-- ----------------------------------------------------------------------------
instance Arbitrary Operator where
  arbitrary = gen <$> arbitrary
    where
      gen :: Int -> Operator
      gen i = Operator $
              toEnum $ mod (abs i)
                            (fromEnum (maxBound :: Game.Make10.Operator) + 1)
-- -}
