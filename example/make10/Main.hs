{-# LANGUAGE      ScopedTypeVariables
                , OverloadedStrings
                , GADTs
                #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Main
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
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
                                , arr
                                , returnA
                                , (>>>)
                                , (<<<)
                                , (>>^)
                                , (&&&)
                                , (|||)
                                )
import Control.Monad            ( replicateM
                                )

import System.Environment
import System.IO

import Text.Read                ( readEither
                                )

import qualified Game.Make10
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
  putStrLn "# Enter the 4 numbers."
  result <- Game.Make10.make10_M_4 10
            <$> map (% (1 :: Integer))
            <$> replicateM 4 ( do
                                  putStr ">>> "
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
       (Kleisli $ const $ parseargs <$> getArgs)
       >>> ((Kleisli print
             >>> (Kleisli $ const $ putStrLn "ERROR! invalid options")) |||
            ((Kleisli $ const $ putStrLn "# Enter the 4 numbers.") &&&
             ((returnA &&&
               ((Kleisli $ const $
                 replicateM 4 (flip runKleisli () $
                               (Kleisli $ const $ putStr ">>> ")
                               >>> (Kleisli $ const $ hFlush stdout)
                               >>> (Kleisli $ const readLn)))
                >>^ map (% 1)))
              >>> arr (uncurry Game.Make10.make_M_4)
              >>> (((Kleisli $ const $ putStr "# length = ") &&&
                    (Kleisli $ print <<< length)) &&&
                   ((Kleisli $ const $ putStr "# result = ") &&&
                    Kleisli print)))
             >>> (Kleisli $ const $ return ())))
       >>> (Kleisli $ const $ hFlush stdout)
       >>> (Kleisli $ const $ hFlush stderr)
  where
    -- -------------------------------------------------------------------------
    parseargs   []      =  Right (10 :: Rational)
    parseargs   [_]     =  Right (10 :: Rational)
    parseargs   (x:xs)
      | x == "-m"       =  readEither $ head xs
      | otherwise       =  parseargs xs
-- -}
