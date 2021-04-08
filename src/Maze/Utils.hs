module Maze.Utils
  ( shuffle
  ) where

import Math.Combinat.Permutations

import System.Random

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle as g =
  let len = length as
      (perm, g') = randomPermutation len g
  in (permuteList perm as, g')
