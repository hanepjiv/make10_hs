{-# LANGUAGE      ScopedTypeVariables
                , OverloadedStrings
                , GADTs
                #-}
-- =============================================================================
-- -----------------------------------------------------------------------------
{-|
Module      : Make10.Expand
Description : puzzle game
Copyright   : (c) hanepjiv, 2015
License     : BSD3
Maintainer  : hanepjiv@gmail.com
Stability   : experimental
Portability : portable

make10, 10-puzzle
-}
module Make10.Expand    ( Expand(..)
                        , optimize
                        , add
                        , sub
                        , mul
                        , truediv
                        ) where
-- =============================================================================
-- -----------------------------------------------------------------------------
import Prelude

import Data.List        ( sort,
                        )
-- =============================================================================
-- -----------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings

-- =============================================================================
-- -----------------------------------------------------------------------------
-- | expand
--
data Expand a where { ExpandList  :: [a]         -> Expand a
                    ; ExpandTuple :: ([a], [a])  -> Expand a
                    } deriving (Eq, Ord, Show)
-- -----------------------------------------------------------------------------
-- | optimize
--
-- >>> optimize (ExpandTuple ([1,0], [1]))
-- ExpandList [0,1]
--
optimize :: forall a. (Num a, Eq a, Ord a) =>
            Expand a                    -> Expand a
optimize    (ExpandList  x)             =  ExpandList $ sort x
optimize    (ExpandTuple (x, [1]))      =  ExpandList $ sort x
optimize    (ExpandTuple (x, y))        =  ExpandTuple (sort x, sort y)
-- -----------------------------------------------------------------------------
-- | add
--
-- >>> add (ExpandList [0]) (ExpandList [1])
-- ExpandList [0,1]
--
-- >>> add (ExpandList [1]) (ExpandTuple ([1], [1,2]))
-- ExpandTuple ([1,1,2],[1,2])
--
-- >>> add (ExpandTuple ([1], [1,2])) (ExpandList [1])
-- ExpandTuple ([1,1,2],[1,2])
--
-- >>> add (ExpandTuple ([1], [1,2])) (ExpandTuple ([1], [1,2]))
-- ExpandTuple ([1,1],[1,2])
--
-- >>> add (ExpandTuple ([1], [1,2])) (ExpandTuple ([2], [3,4]))
-- ExpandTuple ([2,3,4,4],[3,4,6,8])
--
add :: forall a. (Ord a, Num a) => Expand a -> Expand a -> Expand a

add (ExpandList l) (ExpandList r)                       =
  optimize $ ExpandList $ l ++ r

add (ExpandList l) (ExpandTuple (r0, r1))               =
  optimize $ ExpandTuple ([  x  *   y  | x <- l, y <- r1] ++ r0, r1)

add (ExpandTuple (l0, l1)) (ExpandList r)               =
  optimize $ ExpandTuple (l0 ++ [  x  *   y  | x <- l1, y <- r], l1)

add (ExpandTuple (l0, l1)) (ExpandTuple (r0, r1))
  | l1 == r1    =  optimize $ ExpandTuple (l0 ++ r0, l1)
  | otherwise   =  optimize $ ExpandTuple ([  x  *   y  | x <- l0, y <- r1] ++
                                           [  x  *   y  | x <- l1, y <- r0],
                                           [  x  *   y  | x <- l1, y <- r1])
-- -----------------------------------------------------------------------------
-- | sub
--
-- >>> sub (ExpandList [0]) (ExpandList [1])
-- ExpandList [-1,0]
--
-- >>> sub (ExpandList [1]) (ExpandTuple ([1], [1,2]))
-- ExpandTuple ([-1,1,2],[1,2])
--
-- >>> sub (ExpandTuple ([1], [1,2])) (ExpandList [1])
-- ExpandTuple ([-2,-1,1],[1,2])
--
-- >>> sub (ExpandTuple ([1], [1,2])) (ExpandTuple ([1], [1,2]))
-- ExpandTuple ([-1,1],[1,2])
--
-- >>> sub (ExpandTuple ([1], [1,2])) (ExpandTuple ([2], [3,4]))
-- ExpandTuple ([-4,-2,3,4],[3,4,6,8])
--
sub :: forall a. (Ord a, Num a) => Expand a -> Expand a -> Expand a

sub (ExpandList l) (ExpandList r)                       =
  optimize $ ExpandList $ l ++ [-x | x <- r]

sub (ExpandList l) (ExpandTuple (r0, r1))               =
  optimize $ ExpandTuple ([  x  *   y  | x <- l, y <- r1] ++
                          [ -x | x <- r0], r1)

sub (ExpandTuple (l0, l1)) (ExpandList r)               =
  optimize $ ExpandTuple (l0 ++ [  x * (-y) | x <- l1, y <- r], l1)

sub (ExpandTuple (l0, l1)) (ExpandTuple (r0, r1))
  | l1 == r1    =  optimize $ ExpandTuple (l0 ++ [-x | x <- r0], l1)
  | otherwise   =  optimize $ ExpandTuple ([  x  *   y  | x <- l0, y <- r1] ++
                                           [  x  * (-y) | x <- l1, y <- r0],
                                           [  x  *   y  | x <- l1, y <- r1])
-- -----------------------------------------------------------------------------
-- | mul
--
-- >>> mul (ExpandList [3]) (ExpandList [2])
-- ExpandList [6]
--
-- >>> mul (ExpandList [3]) (ExpandTuple ([3], [1,2]))
-- ExpandTuple ([9],[1,2])
--
-- >>> mul (ExpandTuple ([3], [3,2])) (ExpandList [3])
-- ExpandTuple ([9],[2,3])
--
-- >>> mul (ExpandTuple ([4], [7,2])) (ExpandTuple ([3], [7,2]))
-- ExpandTuple ([12],[4,14,14,49])
--
-- >>> mul (ExpandTuple ([3], [4,2])) (ExpandTuple ([2], [3,4]))
-- ExpandTuple ([6],[6,8,12,16])
--
mul :: forall a. (Ord a, Num a) => Expand a -> Expand a -> Expand a

mul (ExpandList l) (ExpandList r)                       =
  optimize $ ExpandList [  x  *   y  | x <- l, y <- r]

mul (ExpandList l) (ExpandTuple (r0, r1))               =
  optimize $ ExpandTuple ([  x  *   y  | x <- l, y <- r0], r1)

mul (ExpandTuple (l0, l1)) (ExpandList r)               =
  optimize $ ExpandTuple ([  x  *   y  | x <- l0, y <- r], l1)

mul (ExpandTuple (l0, l1)) (ExpandTuple (r0, r1))       =
  optimize $ ExpandTuple ([  x  *   y  | x <- l0, y <- r0],
                          [  x  *   y  | x <- l1, y <- r1])
-- -----------------------------------------------------------------------------
-- | truediv
--
-- >>> truediv (ExpandList [3]) (ExpandList [2])
-- ExpandTuple ([3],[2])
--
-- >>> truediv (ExpandList [3]) (ExpandTuple ([3], [1,2]))
-- ExpandTuple ([3,6],[3])
--
-- >>> truediv (ExpandTuple ([3], [3,2])) (ExpandList [3])
-- ExpandTuple ([3],[6,9])
--
-- >>> truediv (ExpandTuple ([4], [7,2])) (ExpandTuple ([3], [7,2]))
-- ExpandTuple ([8,28],[6,21])
--
-- >>> truediv (ExpandTuple ([3], [4,2])) (ExpandTuple ([2], [3,4]))
-- ExpandTuple ([9,12],[4,8])
--
truediv :: forall a. (Ord a, Num a) => Expand a -> Expand a -> Expand a

truediv (ExpandList l) (ExpandList r)                       =
  optimize $ ExpandTuple (l, r)

truediv (ExpandList l) (ExpandTuple (r0, r1))               =
  optimize $ ExpandTuple ([  x  *   y  | x <- l, y <- r1], r0)

truediv (ExpandTuple (l0, l1)) (ExpandList r)               =
  optimize $ ExpandTuple (l0, [  x  *   y  | x <- l1, y <- r])

truediv (ExpandTuple (l0, l1)) (ExpandTuple (r0, r1))       =
  optimize $ ExpandTuple ([  x  *   y  | x <- l0, y <- r1],
                          [  x  *   y  | x <- l1, y <- r0])
