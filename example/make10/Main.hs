{-# LANGUAGE OverloadedStrings #-}


module Main where


import Prelude

import Data.Ratio               ( (%)
                                )

import Control.Applicative      ( (<$>)
                                )
import Control.Monad            ( replicateM
                                )

import qualified Make10


-- =============================================================================
-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "# input 4 numbers: per line >>>"
  result <- Make10.make10 <$> map (% (1 :: Integer)) <$> replicateM 4 readLn

  putStr "# length = "
  print $ length result

  putStr "# result = "
  print result
