module Main where

import Prelude
import Test.Hspec.Runner
import qualified Spec

main :: IO ()
main = hspec Spec.spec
