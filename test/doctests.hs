{-# LANGUAGE      ScopedTypeVariables
                , OverloadedStrings
                , GADTs
                #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
module Main where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude          ( IO
                        , ($)
                        , flip
                        , const
                        )

import Control.Arrow    ( Kleisli(..)
                        , runKleisli
                        , (>>>)
                        )

import qualified Test.DocTest as DocTest
-- =============================================================================
-- -----------------------------------------------------------------------------
main :: IO ()
main =  flip runKleisli () $
        (Kleisli $ const $ DocTest.doctest [ "-packageghc"
                                           , "-isrc"
                                           , "src/Game/Make10.hs"
                                           ])
        >>>
        (Kleisli $ const $ DocTest.doctest [ "-packageghc"
                                           , "-isrc"
                                           , "-iexample/make10"
                                           , "example/make10/Main.hs"
                                           ])
