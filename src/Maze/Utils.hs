module Maze.Utils
  ( shuffle
  ) where

import Math.Combinat.Permutations

import System.Random
-- import System.Random.Shuffle

-- -- | Like 'shuffle', but returns the generator.
-- shuffleG :: RandomGen gen => [a] -> Int -> gen -> ([a], gen)
-- shuffleG elements len gen =
--   let (is, gens) = rseq len gen
--       es = shuffle elements is
--       gen' = if null gens then gen else last gens
--   in (es, gen')
--     where
--       -- The sequence (r1,...r[n-1]) of numbers such that r[i] is an
--       -- independent sample from a uniform random distribution
--       -- [0..n-i]
--       rseq :: RandomGen gen => Int -> gen -> ([Int], [gen])
--       rseq n = unzip . rseq' (n - 1)
--           where
--             rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
--             rseq' 0 gen = []
--             rseq' i gen = (j, gen) : rseq' (i - 1) gen'
--                 where
--                   (j, gen') = randomR (0, i) gen

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle as g =
  let len = length as
      (perm, g') = randomPermutation len g
  in (permuteList perm as, g')
