{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10
Description : puzzle game
Copyright   : (c) hanepjiv, 2013
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10 ( Operator(..)
              , function
                -- -------------------------------------------------------------
              , Cell(..)
              , apply
              , eval
              , optimize
              , expand
                -- -------------------------------------------------------------
              , select
              , combinations
                -- -------------------------------------------------------------
              , makeN
              , make10
              ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Control.Applicative
import Control.Arrow
import Control.Monad ( replicateM
                     )

import Make10.Operator
import Make10.Cell
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings

-- =============================================================================
-- -----------------------------------------------------------------------------
-- | select
--
-- >>> select []
-- []
--
-- >>> select [0,1]
-- [(0,[1]),(1,[0])]
--
-- >>> select [0,1,2]
-- [(0,[1,2]),(1,[0,2]),(2,[0,1])]
--
select :: forall c. [c] -> [(c, [c])]
{- 0.0 -- ----------------------------------------------------------------------
-- UNUSED
select []       = []
select (x:xs)   = (x,xs) : map (\ (y,ys) -> (y,x:ys)) (select xs)
-- -}
{- 0.1 -- ----------------------------------------------------------------------
-- UNUSED
select []       = []
select (x:xs)   = (x,xs) : map (second ((:) x))       (select xs)
-- -}
{- 0.2 -- ----------------------------------------------------------------------
-- UNUSED
select =
  loop $ (snd &&& fst >>> app) &&&
  ((<<< isNull)
   <<< (const [] |||)
   <<< (uncurry (:) <<<)
   <<< (<<< (head &&& tail))
   <<< (id &&&)
   <<< (uncurry map <<<)
   <<< ((second <<< (:) <<< fst) &&&)
   <<< (<<< snd)
   <<< snd)
  where
    -- -------------------------------------------------------------------------
    isNull :: [a] -> Either [a] [a]
    isNull [] = Left  []
    isNull x  = Right x
-- -}
{- 0.3 -- ----------------------------------------------------------------------
-- USED -}
select []       = []
select x        = select_ x
  where
    select_ =
      loop $ (snd &&& fst >>> app) &&&
      ((uncurry (:) <<<)
       <<< (<<< (head &&& tail))
       <<< (id &&&)
       <<< (uncurry map <<<)
       <<< ((second <<< (:) <<< fst) &&&)
       <<< (<<< snd)
       <<< const select)
-- -}
-- =============================================================================
-- -----------------------------------------------------------------------------
-- | combinations
--
-- >>> combinations 1 []
-- []
--
-- >>> combinations 1 [0,1]
-- [[0],[1]]
--
-- >>> combinations 2 [0,1,2]
-- [[0,1],[0,2],[1,2]]
    --
combinations :: forall t. Integer -> [t] -> [[t]]
{- 0.0 -- ----------------------------------------------------------------------
-- UNUSED
combinations _ []     = []
combinations 0 _      = []
combinations 1 x      = map (: []) x
combinations n (x:xs) = map (x:) (combinations (pred n) xs) ++ combinations n xs
-- -}
{- 0.1 -- ----------------------------------------------------------------------
-- UNUSED
combinations =
  curry $ loop $ (snd &&& fst >>> app) &&&
  ((<<< isSndNull)
   <<< (const [] |||)
   <<< (<<< isFstZero)
   <<< (const [] |||)
   <<< (<<< isFstOne)
   <<< ((map (: []) <<< snd) |||)
   <<< (uncurry (++) <<<)
   <<< uncurry (&&&)
   <<< (((uncurry map <<<)
         <<< (((:) <<< head <<< snd) &&&)
         <<< (<<< ((pred <<< fst) &&& (tail <<< snd))))
        &&& (<<< (fst &&& (tail <<< snd))))
   <<< snd)
  where
    -- -------------------------------------------------------------------------
    isSndNull        :: forall t1 t2.
                        (t1, [t2])      -> Either (t1, [t2]) (t1, [t2])
    isSndNull           a@(_, [])       =  Left  a
    isSndNull           a               =  Right a
    -- -------------------------------------------------------------------------
    isFstZero        :: forall t1 t2. (Num t1, Eq t1) =>
                        (t1, t2)        -> Either (t1, t2) (t1, t2)
    isFstZero           a@(0, _)        =  Left  a
    isFstZero           a               =  Right a
    -- -------------------------------------------------------------------------
    isFstOne         :: forall t1 t2. (Num t1, Eq t1) =>
                        (t1, t2)        -> Either (t1, t2) (t1, t2)
    isFstOne            a@(1, _)        =  Left  a
    isFstOne            a               =  Right a
-- -}
{- 0.2 -- ----------------------------------------------------------------------
-- USED -}
combinations _ [] = []
combinations 0 _  = []
combinations 1 x  = map (: []) x
combinations n x  = comb (n, x)
  where
    comb =
      loop $ (snd &&& fst >>> app) &&&
      ((uncurry (++) <<<)
       <<< uncurry (&&&)
       <<< (((uncurry map <<<)
             <<< (((:) <<< head <<< snd) &&&)
             <<< (<<< ((pred <<< fst) &&& (tail <<< snd))))
            &&& (<<< (fst &&& (tail <<< snd))))
       <<< const (uncurry combinations))
-- -}
-- =============================================================================
-- -----------------------------------------------------------------------------
-- | makeTripleA
--
makeTripleA :: forall a.
               (Show a, Ord a, Fractional a) =>
               a -> a -> a -> a -> Operator -> Operator -> Operator -> Cell a
makeTripleA n0 n1 n2 n3 o0 o1 o2 =
  optimize $
  Triple o2 (Triple o1 (Triple o0 (Atom n3) (Atom n2)) (Atom n1)) (Atom n0)
-- -----------------------------------------------------------------------------
-- | makeTripleB
--
makeTripleB :: forall a.
               (Show a, Ord a, Fractional a) =>
               a -> a -> a -> a -> Operator -> Operator -> Operator -> Cell a
makeTripleB n0 n1 n2 n3 o0 o1 o2 =
  optimize $
  Triple o2 (Triple o1 (Atom n3) (Atom n2)) (Triple o0 (Atom n1) (Atom n0))
-- -----------------------------------------------------------------------------
-- | patternA
--
patternA :: [[Integer]]
patternA =  [ [0, 1, 2, 3]
            , [0, 2, 1, 3]
            , [0, 3, 1, 2]
            , [1, 0, 2, 3]
            , [1, 2, 0, 3]
            , [1, 3, 0, 2]
            , [2, 0, 1, 3]
            , [2, 1, 0, 3]
            , [2, 3, 0, 1]
            , [3, 0, 1, 2]
            , [3, 1, 0, 2]
            , [3, 2, 0, 1]
            ]
-- -----------------------------------------------------------------------------
-- | patternB
--
patternB :: [[Integer]]
patternB  = [ [0, 1, 2, 3]
            , [0, 2, 1, 3]
            , [0, 3, 1, 2]
            ]
-- -----------------------------------------------------------------------------
-- | makeTriple
--
makeTriple :: forall a.
              (Show a, Ord a, Fractional a) =>
              [a] -> [Operator] -> [Cell a]
makeTriple ns os =
  filter (not . hasZeroDiv)
  $ map (gen makeTripleA ns os) patternA ++ map (gen makeTripleB ns os) patternB
  where
    gen make_ n_ o_ i_ = make_
                         (n_ !! fromInteger (head i_))
                         (n_ !! fromInteger (i_ !! 1))
                         (n_ !! fromInteger (i_ !! 2))
                         (n_ !! fromInteger (i_ !! 3))
                         (head o_)
                         (o_ !! 1)
                         (o_ !! 2)
-- -----------------------------------------------------------------------------
-- | makeN
--
makeN :: forall a.
          (Show a, Ord a, Fractional a) =>
          a -> [a] -> [Cell a]
makeN n a_in =
  unseen [t | t <- concatMap (makeTriple a_in) $ replicateM (pred (length a_in)) allOp
            , isRightTrue $ (== n) <$> eval t
            ]
  where
    -- -------------------------------------------------------------------------
    isRightTrue (Right True)       = True
    isRightTrue _                  = False
    -- -------------------------------------------------------------------------
    unseen x_ = unseen_ x_ [] []
      where
        unseen_   []     _    _    = []
        unseen_ a@[_]    []   []   = a
        unseen_   (x:xs) seen same
          | x        `elem` seen   = unseen_ xs seen same
          | expand x `elem` same   = unseen_ xs seen same
          | otherwise              = x : unseen_ xs (x:seen) (expand x:same)
-- -----------------------------------------------------------------------------
-- | make10
--
make10 :: forall a.
          (Show a, Ord a, Fractional a) =>
          [a] -> [Cell a]
make10 = makeN 10
