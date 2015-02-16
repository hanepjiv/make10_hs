{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Main
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Main where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Data.Ratio               ( (%)
                                )

import Control.Applicative      ( (<$>)
                                )
import Control.Arrow            ( Kleisli(..)
                                , (>>>)
                                , (<<<)
                                , (&&&)
                                )
import Control.Monad            ( replicateM
                                )

import System.IO

import qualified Make10
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings

-- =============================================================================
-- -----------------------------------------------------------------------------
main :: IO ()
{- 0.0 -- ----------------------------------------------------------------------
-- UNUSED
main = do
  putStrLn "# input 4 numbers"
  result <- Make10.make10
            <$> map (% (1 :: Integer))
            <$> replicateM 4 ( do
                                  putStr "<< "
                                  hFlush stdout
                                  readLn
                             )
  putStr "# length = "
  print $ length result

  putStr "# result = "
  print result
-- -}
{- 0.1 -- ----------------------------------------------------------------------
-- USED -}
main = flip runKleisli () $
       (Kleisli $ const $ putStrLn "# input 4 numbers")
       >>> (Kleisli $ const $ Make10.make10 <$> map (% (1 :: Integer)) <$>
            replicateM 4 (flip runKleisli () $
                          (Kleisli $ const $ putStr "<< ")
                          >>> (Kleisli $ const $ hFlush stdout)
                          >>> (Kleisli $ const readLn)
                         ))
       >>> (((Kleisli $ const $ putStr "# length = ") &&&
             (Kleisli $ print <<< length)) &&&
            ((Kleisli $ const $ putStr "# result = ") &&&
             Kleisli print))
       >>> (Kleisli $ const $ hFlush stdout)
       >>> (Kleisli $ const $ hFlush stderr)
-- -}
