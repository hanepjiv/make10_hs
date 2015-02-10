{-# LANGUAGE OverloadedStrings #-}


module Main where


import Prelude
import qualified Make10


-- =============================================================================
-- -----------------------------------------------------------------------------
main :: IO()
main =  print $ Make10.Atom (1 :: Rational)
