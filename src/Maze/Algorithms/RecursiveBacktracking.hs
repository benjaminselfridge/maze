module Maze.Algorithms.RecursiveBacktracking
  ( recursiveBacktracking
  ) where

import Maze.Core
import Maze.Utils

import Control.Monad ( forM_, when )
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Word
import System.Random

-- | Build a maze using the recursive backtracking algorithm.
recursiveBacktracking :: RandomGen g => g -> Word32 -> Word32 -> (IMaze, g)
recursiveBacktracking g rows cols = runST $ do
  gRef         <- newSTRef g
  maze         <- newSTMaze rows cols
  mazeBounds   <- stMazeBounds maze
  cellsVisited <- newArray mazeBounds False
  let (coord:_) = range mazeBounds
  recursiveBacktracking' gRef maze coord cellsVisited
  imaze        <- freezeSTMaze maze
  g'           <- readSTRef gRef
  return (imaze, g')
  where recursiveBacktracking' :: RandomGen g
                               => STRef s g
                               -> STMaze s
                               -> Coord
                               -- ^ Position we are currently at in the maze.
                               -> STArray s Coord Bool
                               -- ^ Table telling us whether we have visited
                               -- each coordinate yet.
                               -> ST s ()
        recursiveBacktracking' gRef maze pos cellsVisited = do
          -- Mark this coordinate as visited.
          writeArray cellsVisited pos True
          -- Get the neighbors of this cell.
          neighbors <- stMazeNeighborCoords maze pos
          -- Shuffle the list of neighbors, writing back the new random
          -- generator when we're done.
          g <- readSTRef gRef
          let (neighbors', g') = shuffle neighbors g
          writeSTRef gRef g'
          -- For each neighbor, if it has not been visited already, visit it by removing
          -- the wall between the current cell and the neighbor, and be recursively
          -- calling @recursiveBacktracking'@ on the neighbor.
          forM_ neighbors' $ \(dir, nPos) -> do
            visited <- readArray cellsVisited nPos
            when (not visited) $ do
              stMazeOpenCoordDir maze pos dir
              recursiveBacktracking' gRef maze nPos cellsVisited
