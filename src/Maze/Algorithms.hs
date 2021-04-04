module Maze.Algorithms
  ( recursiveBacktracking
  ) where

import Maze.Core

import Control.Monad (filterM, forM_, when)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Word
import System.Random
import System.Random.Shuffle

-- | Build a maze using the recursive backtracking algorithm.
recursiveBacktracking :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
recursiveBacktracking g rows cols = runST $ do
  gRef <- newSTRef g
  maze <- newSTMaze rows cols
  cellsVisited <- newArray ((0, 0), (rows-1, cols-1)) False
  rb gRef maze (0,0) Nothing cellsVisited
  imaze <- freezeSTMaze maze
  g' <- readSTRef gRef
  return (imaze, g')

-- | Like 'shuffle\'', but returns the generator.
shuffleG :: RandomGen gen => [a] -> Int -> gen -> ([a], gen)
shuffleG elements len gen =
  let (is, gens) = rseq len gen
      es = shuffle elements is
      gen' = if null gens then gen else last gens
  in (es, gen')
    where
      -- The sequence (r1,...r[n-1]) of numbers such that r[i] is an
      -- independent sample from a uniform random distribution
      -- [0..n-i]
      rseq :: RandomGen gen => Int -> gen -> ([Int], [gen])
      rseq n = unzip . rseq' (n - 1)
          where
            rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
            rseq' 0 gen = []
            rseq' i gen = (j, gen) : rseq' (i - 1) gen'
                where
                  (j, gen') = randomR (0, i) gen

rb :: RandomGen g
   => STRef s g
   -> STMaze s
   -> Coord
   -- ^ Position we are currently at in the maze.
   -> Maybe Direction
   -- ^ The direction we traveled to get to this position (so @Just Up@ means we
   -- came from the cell below this one, and @Nothing@ means this is the
   -- starting cell).
   -> STArray s Coord Bool
   -- ^ Table telling us whether we have visited a coordinate yet with this
   -- function.
   -> ST s ()
rb gRef maze pos mDir cellsVisited = do
  -- Mark this coordinate as visited.
  writeArray cellsVisited pos True
  -- Get the neighbors of this cell.
  neighbors <- stMazeNeighbors maze pos
  -- Shuffle the list of neighbors.
  g <- readSTRef gRef
  let (neighbors', g') = shuffleG neighbors (length neighbors) g
  writeSTRef gRef g'
  -- For each neighbor, if it has not been visited already, visit it by removing
  -- the wall between the current cell and the neighbor, and be recursively
  -- calling @rb@ on the neighbor.
  forM_ neighbors' $ \(dir, nPos) -> do
    visited <- readArray cellsVisited nPos
    when (not visited) $ do
      stMazeOpen maze pos dir
      rb gRef maze nPos (Just dir) cellsVisited
