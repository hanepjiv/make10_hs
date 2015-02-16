{-# LANGUAGE OverloadedStrings #-}


module Main where


import Prelude

import Data.Ratio               ( (%)
                                )

import Control.Applicative      ( (<$>)
                                , (<*>)
                                )
import Control.Arrow            ( (>>>)
                                , (<<<)
                                )
import Control.Monad            ( replicateM
                                )

import System.IO

import qualified Make10


-- =============================================================================
-- -----------------------------------------------------------------------------
main :: IO ()
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
